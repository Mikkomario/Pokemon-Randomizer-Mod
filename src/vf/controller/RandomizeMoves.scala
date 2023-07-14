package vf.controller

import com.dabomstew.pkrandom.pokemon._
import com.dabomstew.pkrandom.romhandlers.RomHandler
import com.dabomstew.pkrandom.{RandomSource, Settings}
import utopia.flow.collection.CollectionExtensions._
import utopia.flow.collection.immutable.Pair
import utopia.flow.collection.immutable.range.{HasInclusiveEnds, Span}
import vf.model.PokeStat.{Attack, SpecialAttack}
import vf.model.TypeRelation.{Relative, StrongRelative, Unrelated, WeakRelative}
import vf.model._
import vf.util.RandomUtils._

import java.util
import scala.annotation.tailrec
import scala.collection.mutable
import scala.jdk.CollectionConverters._

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
	
	private val swapMoveChance = 0.4
	private val sameTypeChance = 0.4
	private val sameCategoryChance = 0.4
	// Applied when attack and special attack -ratio is modified
	private val decreasedSameCategoryChance = 0.15
	// Threshold before considering attack vs. special attack modified
	private val attackModifierDifferenceThreshold = 0.2
	// This value is applied in reduction. E.g. at least 100% - X% power
	// Power increase is stronger (e.g. -50% converts to +100%)
	private val swapMaxPowerDifference = 0.35
	
	private val addedTypeMoveChance = 0.75
	private val ownTypeMoveChance = 0.5
	private val typeWeights = Map[TypeRelation, Double](
		StrongRelative -> 6.0, Relative -> 4.5, WeakRelative -> 3.0, Unrelated -> 1.5)
	
	
	// OTHER    -------------------------
	
	// Randomizes moves for all pokemon
	def all(typeConversions: Map[Int, Map[Type, Type]], addedTypes: Map[Int, Type],
	        statModifiers: Map[Int, Map[PokeStat, Double]], statSwaps: Map[Int, Map[PokeStat, PokeStat]])
	       (implicit rom: RomHandler, pokemons: Pokemons, moves: Moves, settings: Settings) =
	{
		// Stores moves in java data structures because of the rom interface
		val originalMovesLearnt = rom.getMovesLearnt
		val newMovesBuilder = new java.util.HashMap[Integer, java.util.List[MoveLearnt]]()
		// Randomizes moves for all pokemon
		originalMovesLearnt.keySet().iterator().asScala.foreach { pokeNum =>
			val number: Int = pokeNum
			pokemons(number).foreach { poke =>
				val moveListBuilder = new util.ArrayList[MoveLearnt]()
				apply(poke, originalMovesLearnt.get(pokeNum).iterator().asScala.toVector,
					typeConversions.getOrElse(number, Map()), addedTypes.get(number),
					statModifiers.getOrElse(number, Map()), statSwaps.getOrElse(number, Map()))
					.foreach { case (level, moveNumber) =>
						val learnt = new MoveLearnt()
						learnt.move = moveNumber
						learnt.level = level
						moveListBuilder.add(learnt)
					}
				newMovesBuilder.put(pokeNum, moveListBuilder)
			}
		}
		// Makes sure cosmetic forms have the same moves as their base forms
		pokemons.cosmeticForms.foreach { cosmeticPoke =>
			if (originalMovesLearnt.containsKey(cosmeticPoke.number: Integer) &&
				originalMovesLearnt.containsKey(cosmeticPoke.baseForme.number: Integer))
			{
				// TODO: Might want to copy the MoveLearnt instances, also
				val copyList = new java.util.ArrayList(newMovesBuilder.get(cosmeticPoke.baseForme.number))
				newMovesBuilder.put(cosmeticPoke.number, copyList)
			}
		}
		// Applies the new moves
		rom.setMovesLearnt(newMovesBuilder)
	}
	
	// Returns new moves to assign (level -> move number)
	private def apply(poke: Pokemon, originalMovesLearnt: Seq[MoveLearnt],
	          typeConversions: Map[Type, Type], addedType: Option[Type],
	          statModifiers: Map[PokeStat, Double], statSwaps: Map[PokeStat, PokeStat])
	         (implicit moves: Moves, settings: Settings, rom: RomHandler) =
	{
		val physicalToSpecialRatio = poke.getAttackSpecialAttackRatio
		val currentTypes = TypeSet.from(poke)
		val currentRelations = TypeRelations.of(currentTypes)
		// Whether attack and special attack have been so modified that moves need to be altered
		val categoriesChanged = statSwaps.contains(Attack) || statSwaps.contains(SpecialAttack) ||
			(math.abs(statModifiers.getOrElse(Attack, 1.0) - statModifiers.getOrElse(SpecialAttack, 1.0))
				>= attackModifierDifferenceThreshold)
		val actualSameCategoryChance = if (categoriesChanged) decreasedSameCategoryChance else sameCategoryChance
		
		// Contains move numbers of all already picked moves
		val pickedMovesBuilder = mutable.Set[Int]()
		
		// Generates a new move
		def newMove() = {
			// Prefers added type, if applicable
			// Secondarily, prefers own type
			// Thirdly, selects from related types
			val moveType = addedType.filter { _ => RandomSource.nextDouble() < addedTypeMoveChance }
				.getOrElse {
					if (chance(ownTypeMoveChance))
						currentTypes.random
					else
						currentRelations.random(typeWeights)
				}
			val category = randomCategoryIn(moveType, physicalToSpecialRatio)
			randomMove(pickedMovesBuilder, moveType, category)
		}
		// Finds a relative random move
		def swapMove(original: Move) = {
			// When selecting replacing move type, takes into consideration if the pokemon swapped type
			// i.e. STAB moves will still remain STAB moves when they preserve their type
			val originalMoveType = typeConversions.getOrElse(original.`type`, original.`type`)
			val moveType = {
				// Case: Move preserves its type
				if (chance(sameTypeChance))
					originalMoveType
				// Case: STAB move
				else if (chance(ownTypeMoveChance))
					currentTypes.random
				// Case: Other type (relative to the original move type)
				else
					TypeRelations.of(originalMoveType).random(typeWeights)
			}
			val moveCategory = {
				// Case: Move preserves category
				if (chance(actualSameCategoryChance))
					original.category
				// Case: Move acquires random category
				else
					randomCategoryIn(moveType, physicalToSpecialRatio)
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
			randomMove(pickedMovesBuilder, moveType, moveCategory, powerRange)
		}
		// Preserves the previously selected move, if possible
		def keepMove(original: Move) = {
			// Case: Can't keep the original move because of type change, overlap or stat change => Swaps to a new move
			if ((categoriesChanged && original.category != MoveCategory.STATUS) ||
				typeConversions.contains(original.`type`) ||
				pickedMovesBuilder.contains(original.number))
				swapMove(original)
			// Case: Keeps the original move
			else {
				pickedMovesBuilder += original.number
				original.number
			}
		}
		def swapOrKeep(original: Move) = if (chance(swapMoveChance)) swapMove(original) else keepMove(original)
		
		// Assigns a certain number of starting and evo moves (based on settings)
		val originalEvoMoves = originalMovesLearnt.takeWhile { _.level == 0 }.map { _.move }
		val originalStartingMoves = originalMovesLearnt.view
			.drop(originalEvoMoves.size).takeWhile { _.level == 1 }.map { _.move }.toVector
		
		val evoMoves = {
			// Case: Already has evo moves => Possibly swaps them
			if (originalEvoMoves.nonEmpty)
				originalEvoMoves.map { move => swapOrKeep(moves.byNumber(move)) }
			// Case: No existing evo moves => May add (based on settings)
			else if (settings.isEvolutionMovesForAll)
				Vector(newMove())
			// Case: Not adding new moves
			else
				Vector()
		}
		val startingMoves = {
			val existing = originalStartingMoves.map { move => swapOrKeep(moves.byNumber(move)) }
			val minMoveCount = settings.getGuaranteedMoveCount
			if (existing.hasSize < minMoveCount)
				existing ++ Vector.fill(minMoveCount - existing.size) { newMove() }
			else
				existing
		}
		
		// Swaps (some of) the original moves
		val newDefaultMoves = originalMovesLearnt.drop(originalEvoMoves.size + originalStartingMoves.size)
			.map { learnt => learnt.level -> swapOrKeep(moves.byNumber(learnt.move)) }
		
		// Assigns new moves to the between-levels
		val newMoveLevels = {
			if (extraMoveRatio > 0)
				newDefaultMoves.iterator.map { _._1 }.paired
					.flatMap { case Pair(previous, next) =>
						distinctNextInts(extraMoveRatio, next - previous - 1).map { previous + _ + 1 }
					}
					.toVector.sorted
			// Case: Feature disabled
			else
				Vector()
		}
		val (newNonDamagingMoves, newDamagingMoves) = Vector.fill(newMoveLevels.size) { moves.byNumber(newMove()) }
			.divideBy { _.power > 0 }
		// Orders these new moves by power
		// Non-power moves are placed randomly
		val newMovesBuffer = mutable.Buffer.from(newDamagingMoves.sortBy { m => m.power * m.hitCount * m.hitratio })
		newNonDamagingMoves.foreach { move =>
			newMovesBuffer.insert(RandomSource.nextInt(newMovesBuffer.size), move)
		}
		
		// Returns the combined move-list
		evoMoves.map { 0 -> _ } ++ startingMoves.map { 1 -> _ } ++
			(newDefaultMoves ++ newMoveLevels.iterator.zip(newMovesBuffer)
				.map { case (level, move) => level -> move.number })
				.sortBy { _._1 }
	}
	
	// Selects from physical vs. special based on stats
	// May also select status type
	def randomCategoryIn(moveType: Type, physicalToSpecialRatio: Double)(implicit moves: Moves) = {
		if (RandomSource.nextDouble() < moves.statusMoveRatioByType(moveType))
			MoveCategory.STATUS
		else if (RandomSource.nextDouble() < physicalToSpecialRatio)
			MoveCategory.PHYSICAL
		else
			MoveCategory.SPECIAL
	}
	
	// Randomly selects the move from available options
	private def randomMove(pickedMovesBuilder: mutable.Set[Int], moveType: Type, category: MoveCategory,
	                       powerRange: Option[HasInclusiveEnds[Double]] = None)
	                      (implicit moves: Moves): Int =
		randomMove(Some(moveType), Some(category), powerRange, pickedMovesBuilder)
	
	@tailrec
	private def randomMove(moveType: Option[Type], category: Option[MoveCategory],
	                       powerRange: Option[HasInclusiveEnds[Double]], pickedMovesBuilder: mutable.Set[Int])
	                      (implicit moves: Moves): Int =
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
					case None => moves.all
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
					randomMove(None, category, powerRange, pickedMovesBuilder)
				// 2) Removes power filter, if possible
				else
					randomMove(moveType, category, None, pickedMovesBuilder)
			}
			// 3) Eases the category filter, if possible
			else if (category.isDefined)
				randomMove(moveType, None, powerRange, pickedMovesBuilder)
			// 4) Eases the type filter, if possible
			else if (moveType.isDefined)
				randomMove(None, category, powerRange, pickedMovesBuilder)
			// Case: Totally out of moves (unlikely) => Selects just any move
			else
				moves.all.head.number
		}
		// Case: Valid set of available moves => Picks one randomly
		else {
			val index = RandomSource.nextInt(finalOptions.size)
			val result = finalOptions.iterator.drop(index).next()
			// Remembers that this move was picked
			pickedMovesBuilder += result
			result
		}
	}
}
