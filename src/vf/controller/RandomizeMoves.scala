package vf.controller

import com.dabomstew.pkrandom.constants.Species
import com.dabomstew.pkrandom.pokemon._
import com.dabomstew.pkrandom.romhandlers.RomHandler
import com.dabomstew.pkrandom.{RandomSource, Settings}
import utopia.flow.collection.CollectionExtensions._
import utopia.flow.collection.immutable.Pair
import utopia.flow.collection.immutable.caching.cache.Cache
import utopia.flow.collection.immutable.range.{HasInclusiveOrderedEnds, NumericSpan, Span}
import utopia.flow.operator.enumeration.End.{First, Last}
import vf.model.TypeRelation.{Relative, StrongRelative, Unrelated, WeakRelative}
import vf.model._
import vf.poke.core.model.cached.{Spread, SpreadThresholds, SpreadValues}
import vf.poke.core.model.enumeration.PokeType
import vf.poke.core.model.enumeration.Stat.{Attack, SpecialAttack}
import vf.util.RandomUtils._
import vf.util.PokeExtensions._
import vf.util.RandomUtils

import java.io.PrintWriter
import java.util
import scala.annotation.tailrec
import scala.collection.immutable.VectorBuilder
import scala.collection.mutable

/**
 * Randomizes pokemon move-sets. Also adds new moves.
 * @author Mikko Hilpinen
 * @since 8.7.2023, v1.0-alt
 */
object RandomizeMoves
{
	// ATTRIBUTES   ---------------------
	
	// 0 = no extra moves, 1 = 100% extra moves, 2 = 200% extra moves, and so on...
	private val extraMoveRatio = 1
	private val minMovesCount = 12
	private val additionalMovesPerFavouriteLevel = 4
	
	private val swapMoveChance = 0.4
	private val swapRareMoveChance = 0.2
	private val sameTypeChance = 0.4
	private val sameCategoryChance = 0.4
	// Applied when attack and special attack -ratio is modified
	private val decreasedSameCategoryChance = 0.15
	// Threshold before considering attack vs. special attack modified
	// private val attackModifierDifferenceThreshold = 0.2
	// This value is applied in reduction. E.g. at least 100% - X% power
	// Power increase is stronger (e.g. -50% converts to +100%)
	private val swapMaxPowerDifference = 0.35
	private val maxStartingMovePower = 50
	
	private val addedTypeMoveChance = 0.75
	private val ownTypeMoveChance = 0.5
	private val typeWeights = Map[TypeRelation, Double](
		StrongRelative -> 9.0, Relative -> 5.0, WeakRelative -> 3.0, Unrelated -> 1.0)
	
	// Affects the chance to gain an advantage against otherwise superior type
	private val defensiveBuffWeightMod = 0.5
	// Affects the chance to gain an advantage against a type which the STAB types are not effective against
	// Not combined with 'defensiveBuffWeightMod'
	private val offensiveBuffWeightMod = 0.8
	// An favourite level -based increase sometimes applied to defensive and offensive weight modifiers
	private val buffWeightIncreasePerFavouriteLevel = 0.1
	
	private val levelSpreadWeightMods = SpreadValues(0.25, 1.0, 1.5, 0.5)
	private val appearanceRateImpactMod = 0.5
	private val appearanceRateWeightModRange = NumericSpan(0.2, 3.0)
	private val rareMoveAppearanceRateThreshold = 0.2
	
	
	// OTHER    -------------------------
	
	// Randomizes moves for all pokemon
	// Returns the assigned moves for each poke
	def all(groups: Iterable[EvolveGroup], minAppearanceLevels: Map[EvolveGroup, Int])
	       (implicit rom: RomHandler, pokes: Pokes, moves: Moves, settings: Settings) =
	{
		Log("moves") { writer =>
			// Records move level spreads and appearance rates
			val moveLevelWeightSpreads = moveLevelSpreadMapFrom(groups).view
				.mapValues { _ + levelSpreadWeightMods }.toMap
			val appearanceRates = moveAppearanceRatesFrom(groups)
			// Stores moves in java data structures because of the rom interface
			val originalMovesLearnt = rom.getMovesLearnt
			val newMovesBuilder = new java.util.HashMap[Integer, java.util.List[MoveLearnt]]()
			// Randomizes moves for all pokemon
			val movesPerPoke = groups.flatMap { group =>
				val minAppearanceLevel = minAppearanceLevels.get(group)
				group.levelThresholds.map { case (poke, evolveLevel) =>
					// Takes a note of the first level at which this poke realistically appears
					// Either after evolve or in the wild (if a randomized wild poke)
					val firstLevel = minAppearanceLevel match {
						case Some(minAppearanceLevel) => evolveLevel max minAppearanceLevel
						case None => evolveLevel
					}
					val moveListBuilder = new util.ArrayList[MoveLearnt]()
					val movesLearnt = apply(poke, firstLevel, group.favouriteLevel, moveLevelWeightSpreads, appearanceRates,
						writer)
					movesLearnt.foreach { move => moveListBuilder.add(move.toMoveLearnt) }
					newMovesBuilder.put(poke.number, moveListBuilder)
					poke -> movesLearnt
				}
			}.toMap
			// Makes sure cosmetic forms have the same moves as their base forms
			pokes.cosmeticForms.foreach { cosmeticPoke =>
				if (originalMovesLearnt.containsKey(cosmeticPoke.number: Integer) &&
					originalMovesLearnt.containsKey(cosmeticPoke.baseForme.number: Integer))
				{
					val copyList = new java.util.ArrayList(newMovesBuilder.get(cosmeticPoke.baseForme.number))
					newMovesBuilder.put(cosmeticPoke.number, copyList)
				}
			}
			// Finally, makes sure all pokes are covered
			val missingPokes = pokes.flatMap { poke =>
				if (newMovesBuilder.containsKey(poke.number: Integer))
					None
				else {
					val moveListBuilder = new util.ArrayList[MoveLearnt]()
					apply(poke, 0, 0, moveLevelWeightSpreads, appearanceRates, writer)
						.foreach { move => moveListBuilder.add(move.toMoveLearnt) }
					newMovesBuilder.put(poke.number, moveListBuilder)
					Some(poke)
				}
			}
			if (missingPokes.nonEmpty) {
				writer.println(s"\nWARNING: The following ${missingPokes.size} pokes didn't receive moves by default:")
				missingPokes.foreach { p => writer.println(s"\t- ${p.wrapped.fullName} (#${p.number})") }
			}
			// Applies the new moves
			rom.setMovesLearnt(newMovesBuilder)
			pokes.foreach { _.updateState() }
			movesPerPoke
		}
	}
	
	// Returns new moves to assign (level -> move number)
	private def apply(poke: Poke, firstLevel: Int, favouriteLevel: Int, levelWeightMods: Map[Int, Spread[Int, Double]],
	                  moveAppearanceRates: Map[PokeType, Pair[Map[Int, Double]]], writer: PrintWriter)
	                 (implicit moves: Moves, settings: Settings, rom: RomHandler): Vector[MoveLearn] =
	{
		// Adds weight modifiers to types based on how they affect this poke's offensive and defensive capabilities
		val typeEffectiveness = poke.types.effectiveness
		val defensiveWeaknesses = typeEffectiveness.defensiveWeaknesses
		val offensiveWeaknesses = typeEffectiveness.offensiveWeaknesses
		val additionalTypeWeights = PokeType.values.iterator
			.map { attackType =>
				val attackEffectiveness = EffectivenessRelations(attackType)
				val defenseImprovement = defensiveWeaknesses
					.count { attackerType => attackEffectiveness.offenseRatingAgainst(attackerType) > 0 }
				val weight = {
					if (defenseImprovement > 0)
						math.pow(defensiveBuffWeightMod, defenseImprovement)
					else {
						val offenseImprovement = offensiveWeaknesses
							.count { defenderType => attackEffectiveness.offenseRatingAgainst(defenderType) > 0 }
						if (offenseImprovement > 0)
							math.pow(offensiveBuffWeightMod, offenseImprovement)
						else
							1.0
					}
				}
				attackType -> (weight + buffWeightIncreasePerFavouriteLevel * favouriteLevel)
			}
			// Won't add a weight modifier to own types
			.toMap -- poke.types.types
		
		writer.println(s"\nProcessing ${poke.name} (${poke.types} / ${
			if (poke(Attack) > poke(SpecialAttack)) "Physical" else "Special" } (${(poke.attackSpAttackRatio * 100).toInt}% physical))\t----------------")
		writer.println(s"\t- Applies the following type weight modifiers, in addition to type relations:")
		additionalTypeWeights.filterNot { _._2 == 1.0 }.foreach { case (t, wt) => writer.println(s"\t\t- $t: $wt") }
		
		val typeConversions = poke.typeSwaps
		val currentRelations = poke.types.relations
		// Whether attack and special attack have been so modified that moves need to be altered
		val categoriesChanged = poke.attackToSpAttackRatioHasReversed
		val actualSameCategoryChance = if (categoriesChanged) decreasedSameCategoryChance else sameCategoryChance
		
		// Contains move numbers of all already picked moves
		val pickedMovesBuilder = mutable.Set[Int]()
		
		// Caches move appearance rates
		val moveAppearanceRateCache = Cache { move: Move =>
			val moveType = move.`type`.toScala
			moveAppearanceRates(moveType).apply(if (poke.types.contains(moveType)) Last else First)
				.getOrElse(move.number, 1.0)
		}
		
		// Generates a new move
		def newMove(targetLevel: Int) = {
			// Prefers added type, if applicable
			// Secondarily, prefers own type
			// Thirdly, selects from related types
			val moveType = poke.addedType.filter { _ => RandomSource.nextDouble() < addedTypeMoveChance }
				.getOrElse {
					if (chance(ownTypeMoveChance))
						poke.types.random
					else
						currentRelations.random(typeWeights, additionalTypeWeights)
				}
			val category = randomCategoryIn(moveType, poke.attackSpAttackRatio)
			val powerRange = if (targetLevel <= 1) Some(NumericSpan(0.0, maxStartingMovePower)) else None
			val move = randomMove(targetLevel, moveType, category, powerRange, poke.types, levelWeightMods,
				moveAppearanceRateCache, pickedMovesBuilder)
			writer.println(s"New move: ${move.name}")
			move
		}
		// Finds a relative random move
		def swapMove(original: Move, level: Int) = {
			// When selecting replacing move type, takes into consideration if the pokemon swapped type
			// i.e. STAB moves will still remain STAB moves when they preserve their type
			val originalMoveType = typeConversions.getOrElse(original.`type`: PokeType, original.`type`: PokeType)
			val moveType = {
				// Case: Move preserves its type
				if (chance(sameTypeChance))
					originalMoveType
				// Case: STAB move
				else if (chance(ownTypeMoveChance))
					poke.types.random
				// Case: Other type (relative to the original move type)
				else
					TypeRelations.of(originalMoveType).random(typeWeights, additionalTypeWeights)
			}
			val moveCategory = {
				// Case: Move preserves category
				if (chance(actualSameCategoryChance))
					original.category
				// Case: Move acquires random category
				else
					randomCategoryIn(moveType, poke.attackSpAttackRatio)
			}
			// Selects from moves with similar strength, if applicable
			val powerRange = {
				if (original.power > 0) {
					val originalPower = original.power * original.hitratio * original.hitCount
					val diffMod = 1 - swapMaxPowerDifference
					Some(Span.numeric(originalPower * diffMod, originalPower / diffMod))
				}
				else
					None
			}
			val move = randomMove(level, moveType, moveCategory, powerRange, poke.types, levelWeightMods,
				moveAppearanceRateCache, pickedMovesBuilder)
			writer.println(s"Swaps ${original.name} to ${move.name}")
			move
		}
		// Preserves the previously selected move, if possible
		def keepMove(original: Move, level: Int) = {
			// Case: Can't keep the original move because of type change, overlap or stat change => Swaps to a new move
			if ((categoriesChanged && original.category != MoveCategory.STATUS) ||
				typeConversions.contains(original.`type`: PokeType) ||
				pickedMovesBuilder.contains(original.number))
				swapMove(original, level)
			// Case: Keeps the original move
			else {
				pickedMovesBuilder += original.number
				original
			}
		}
		def swapOrKeep(original: Move, level: Int) = {
			// Chances to keep the move are higher for rare moves
			val appliedSwapChance = {
				if (moveAppearanceRateCache(original) <= rareMoveAppearanceRateThreshold)
					swapRareMoveChance
				else
					swapMoveChance
			}
			if (chance(appliedSwapChance)) swapMove(original, level) else keepMove(original, level)
		}
		
		// Assigns a certain number of starting and evo moves (based on settings)
		val originalEvoMoves = poke.originalState.evoMoves
		val originalStartingMoves = poke.originalState.startingMoves
		
		val evoMoves = {
			// Case: Aegislash => Will keep it's evo moves (king's shield)
			if (poke.number == Species.aegislash)
				originalEvoMoves
			// Case: Already has evo moves => Possibly swaps them
			else if (originalEvoMoves.nonEmpty)
				originalEvoMoves.map { move => swapOrKeep(moves.byNumber(move), firstLevel) }.map { _.number }
			// Case: No existing evo moves => May add (based on settings)
			else if (settings.isEvolutionMovesForAll)
				Vector(newMove(firstLevel).number)
			// Case: Not adding new moves
			else
				Vector()
		}
		val startingMoves = {
			val existing = originalStartingMoves.map { move => swapOrKeep(moves.byNumber(move), firstLevel) }
			val minMoveCount = settings.getGuaranteedMoveCount
			if (existing.hasSize < minMoveCount)
				existing ++ Vector.fill(minMoveCount - existing.size) { newMove(firstLevel) }
			else
				existing
		}
		
		// Swaps (some of) the original moves
		val newDefaultMoves = poke.normalMoves.map { learn =>
			learn.mapMove { move => swapOrKeep(moves.byNumber(move), learn.level).number }
		}
		
		// Assigns new moves to the between-levels
		val newMoveLevels = {
			if (extraMoveRatio > 0)
				newDefaultMoves.iterator.map { _.level }.paired
					.flatMap { case Pair(previous, next) =>
						distinctNextInts(extraMoveRatio, next - previous - 1).map { previous + _ + 1 }
					}
					.toVector.sorted
			// Case: Feature disabled
			else
				Vector()
		}
		val (newNonDamagingMoves, newDamagingMoves) = newMoveLevels.map(newMove).divideBy { _.power > 0 }.toTuple
		// Orders these new moves by power
		// Non-power moves are placed randomly
		val newMovesBuffer = mutable.Buffer.from(newDamagingMoves.sortBy { m =>
			val base = m.power * m.hitCount * m.hitratio
			val countingRecharge = if (m.isRechargeMove) base * 0.5 else base
			val countingStatChange = if (m.hasBeneficialStatChange) countingRecharge * 1.15 else countingRecharge
			m.criticalChance match {
				case CriticalChance.GUARANTEED => countingStatChange * 1.6
				case CriticalChance.INCREASED => countingStatChange * 1.1
				case _ => countingStatChange
			}
		})
		newNonDamagingMoves.foreach { move =>
			if (newMovesBuffer.isEmpty)
				newMovesBuffer.append(move)
			else
				newMovesBuffer.insert(RandomSource.nextInt(newMovesBuffer.size), move)
		}
		
		// Combines the moves together and adds additional moves, if needed
		val standardMoves = evoMoves.map(MoveLearn.evo) ++ startingMoves.map { m => MoveLearn.start(m.number) } ++
			(newDefaultMoves ++ newMoveLevels.iterator.zip(newMovesBuffer)
				.map { case (level, move) => MoveLearn(level, move.number) })
		val finalMoves = {
			val realisticMovesCount = standardMoves.count { _.level >= firstLevel }
			val addedMoveCount = ((minMovesCount - realisticMovesCount) max 0) +
				additionalMovesPerFavouriteLevel * favouriteLevel
			// Case: Additional moves are not needed
			if (addedMoveCount <= 0) {
				writer.println(s"=> $realisticMovesCount \"realistic\" moves")
				standardMoves
			}
			// Case: New moves are needed => Generates as many as are needed
			else {
				writer.println(s"=> $realisticMovesCount \"realistic\" moves => Adds $addedMoveCount new moves")
				// Won't place any moves on levels where there are moves already
				val usedLevels = mutable.Set[Int]()
				standardMoves.foreach { usedLevels += _.level }
				val newMovesIter = Iterator
					.continually {
						val level = Iterator
							.continually { firstLevel + RandomSource.nextInt(100 - firstLevel) }
							.filterNot(usedLevels.contains)
							.next()
						val move = newMove(level)
						usedLevels += level
						MoveLearn(level, move.number)
					}
					.take(addedMoveCount)
				standardMoves ++ newMovesIter
			}
		}
		
		// Returns the combined move-list
		finalMoves.sortBy { _.level }
	}
	
	// Selects from physical vs. special based on stats
	// May also select status type
	def randomCategoryIn(moveType: PokeType, physicalToSpecialRatio: Double)(implicit moves: Moves) = {
		if (RandomSource.nextDouble() < moves.statusMoveRatioByType(moveType))
			MoveCategory.STATUS
		else if (RandomSource.nextDouble() < physicalToSpecialRatio)
			MoveCategory.PHYSICAL
		else
			MoveCategory.SPECIAL
	}
	
	// Randomly selects the move from available options
	private def randomMove(targetLevel: Int, moveType: PokeType, category: MoveCategory,
	                       powerRange: Option[HasInclusiveOrderedEnds[Double]] = None, pokeTypes: TypeSet,
	                       levelSpreadMods: Map[Int, Spread[Int, Double]], appearanceRateCache: Cache[Move, Double],
	                       pickedMovesBuilder: mutable.Set[Int])
	                      (implicit moves: Moves): Move =
		_randomMove(targetLevel, Some(moveType), Some(category), powerRange, pokeTypes, levelSpreadMods,
			appearanceRateCache, pickedMovesBuilder)
	
	@tailrec
	private def _randomMove(targetLevel: Int, moveType: Option[PokeType], category: Option[MoveCategory],
	                       powerRange: Option[HasInclusiveOrderedEnds[Double]], pokeTypes: TypeSet,
	                       levelSpreadMods: Map[Int, Spread[Int, Double]], appearanceRateCache: Cache[Move, Double],
	                       pickedMovesBuilder: mutable.Set[Int])
	                      (implicit moves: Moves): Move =
	{
		// Filters move options by type and category
		val filteredOptions: Set[Move] = moveType match {
			case Some(t) =>
				val typeMoves = moves.byType(t)
				category match {
					case Some(c) => typeMoves & moves.byCategory(c)
					case None => typeMoves
				}
			case None =>
				category match {
					case Some(c) => moves.byCategory(c)
					case None => moves.valid
				}
		}
		// Filters out already selected moves
		val finalOptions = {
			// Filters by power range, if applicable
			val inPowerRange = powerRange match {
				case Some(range) => filteredOptions.filter { m => range.contains(m.power * m.hitCount * m.hitratio) }
				case None => filteredOptions
			}
			inPowerRange.map { _.number } -- pickedMovesBuilder
		}
		
		// Case: No moves to select from => Eases the filters
		if (finalOptions.isEmpty) {
			if (powerRange.isDefined) {
				// 1) For power-restricted moves, removes the type filter first, if possible
				if (moveType.isDefined)
					_randomMove(targetLevel, None, category, powerRange, pokeTypes, levelSpreadMods, appearanceRateCache,
						pickedMovesBuilder)
				// 2) Removes power filter, if possible
				else
					_randomMove(targetLevel, moveType, category, None, pokeTypes, levelSpreadMods, appearanceRateCache,
						pickedMovesBuilder)
			}
			// 3) Eases the category filter, if possible
			else if (category.isDefined)
				_randomMove(targetLevel, moveType, None, powerRange, pokeTypes, levelSpreadMods, appearanceRateCache,
					pickedMovesBuilder)
			// 4) Eases the type filter, if possible
			else if (moveType.isDefined)
				_randomMove(targetLevel, None, category, powerRange, pokeTypes, levelSpreadMods, appearanceRateCache,
					pickedMovesBuilder)
			// Case: Totally out of moves (unlikely) => Selects just any move
			else
				moves.all.head
		}
		// Case: Valid set of available moves => Picks one randomly
		else {
			// Assigns a weight to each move based on their appearance rate and level spread
			val weighedOptions = finalOptions.iterator.map { moveNumber =>
				val move = moves.byNumber(moveNumber)
				val levelWeight = levelSpreadMods.get(moveNumber) match {
					case Some(levelWeights) => levelWeights(targetLevel)
					case None => 1.0
				}
				val appearanceWeight = appearanceRateWeightModRange
					.restrict(math.pow(appearanceRateCache(move), appearanceRateImpactMod))
				move -> (levelWeight * appearanceWeight)
			}.toVector
			val result = RandomUtils.weighedRandom(weighedOptions)
			// Remembers that this move was picked
			pickedMovesBuilder += result.number
			result
		}
	}
	
	private def moveLevelSpreadMapFrom(groups: Iterable[EvolveGroup]) = {
		// Records the levels at which each move appears
		val moveLevelsMap = mutable.Map[Int, VectorBuilder[Int]]()
		groups.foreach { group =>
			group.levelThresholds
				.map { case (poke, minLevel) =>
					poke.moves.map { moveLearn =>
						val level = {
							if (moveLearn.isLearnedMove && moveLearn.level > minLevel)
								moveLearn.level
							else
								minLevel
						}
						moveLearn.move -> level
					}.toMap
				}
				.reduceLeft { (a, b) => a.mergeWith(b) { _ min _ } }
				.foreach { case (move, level) => moveLevelsMap.getOrElseUpdate(move, new VectorBuilder()) += level }
		}
		moveLevelsMap.view.mapValues { levelsBuilder =>
			// Forms level spread models based on this data
			SpreadThresholds.from(levelsBuilder.result().sorted)
		}.toMap
	}
	
	// Returns:
	// Two maps for each move type
	//      1) Appearance rates (relative to average) in pokes outside of the moves type (no STAB)
	//      2) Appearance rates in pokes with same type (STAB)
	private def moveAppearanceRatesFrom(groups: Iterable[EvolveGroup])(implicit moves: Moves) =
	{
		// First entries are same type -counts
		// Second values are other type -counts
		// First values are move types. Second values are move ids
		val countMaps = PokeType.values.map { t => t -> Pair.fill(mutable.Map[Int, Int]()) }.toMap
		groups.foreach { group =>
			// Counts the move appearances within and outside of poke types
			// Removes duplicate entries. I.e. only counts +1 for the whole group, not +1 per poke form
			val dividedTypeMoves = group.forms
				.splitFlatMap { poke =>
					poke.moves
						// Skips moves not found within the implicit 'moves' object
						// Also skips moves with unknown type
						.flatMap { moveLearn =>
							val moveNumber = moveLearn.move
							moves.byNumber.get(moveNumber).map { move => move.`type`.toScala -> moveNumber }
						}
						.toSet
						.divideBy { case (t, _) => poke.types.contains(t) }.toTuple
				}
			// Updates the count map for each move
			Pair.tupleToPair(dividedTypeMoves).map { _.toSet }.foreachSide { case (moves, side) =>
				moves.foreach { case (t, moveNumber) =>
					countMaps(t)(side).updateWith(moveNumber) {
						case Some(n) => Some(n + 1)
						case None => Some(1)
					}
				}
			}
		}
		// Processes the count maps so that each move contains a relative appearance rate within and outside of its type
		countMaps.view.mapValues { appearanceMaps =>
			appearanceMaps.map { appearanceMap =>
				val averageAppearanceCount = appearanceMap.valuesIterator.sum / appearanceMap.size.toDouble
				appearanceMap.view.mapValues { _ / averageAppearanceCount }.toMap
			}
		}.toMap
	}
}
