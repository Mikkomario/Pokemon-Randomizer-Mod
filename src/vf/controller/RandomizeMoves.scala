package vf.controller

import com.dabomstew.pkrandom.{RandomSource, Settings}
import com.dabomstew.pkrandom.pokemon.{Move, MoveCategory, MoveLearnt, Pokemon, Type}
import com.dabomstew.pkrandom.romhandlers.RomHandler
import utopia.flow.collection.CollectionExtensions._
import vf.model.{Moves, TypeRelation, TypeRelations, TypeSet}
import vf.model.TypeRelation.{Relative, StrongRelative, Unrelated, WeakRelative}

/**
 * Randomizes pokemon move-sets. Also adds new moves.
 * @author Mikko Hilpinen
 * @since 8.7.2023, v1.0-alt
 */
object RandomizeMoves
{
	// ATTRIBUTES   ---------------------
	
	private val extraMoveRatio = 2
	
	private val swapMoveChance = 0.2
	private val sameTypeChance = 0.4
	private val sameCategoryChance = 0.7
	private val swapMaxPowerDifference = 0.4
	
	private val addedTypeMoveChance = 0.75
	private val ownTypeMoveChance = 0.5
	private val typeWeights = Map[TypeRelation, Double](
		StrongRelative -> 6.0, Relative -> 4.5, WeakRelative -> 3.0, Unrelated -> 1.5)
	
	
	// OTHER    -------------------------
	
	/*
	// Get current sets
			Map<Integer, List<MoveLearnt>> movesets = this.getMovesLearnt();
	 */
	def apply(poke: Pokemon, originalMovesLearnt: Vector[MoveLearnt],
	          typeConversions: Map[Type, Type], addedType: Option[Type])
	         (implicit moves: Moves, settings: Settings, rom: RomHandler) =
	{
		val physicalToSpecialRatio = poke.getAttackSpecialAttackRatio
		val currentTypes = TypeSet.from(poke)
		val currentRelations = TypeRelations.of(currentTypes)
		
		// Selects from physical vs. special based on stats
		// May also select status type
		def randomCategoryIn(moveType: Type) = {
			if (RandomSource.nextDouble() < moves.statusMoveRatioByType(moveType))
				MoveCategory.STATUS
			else if (RandomSource.nextDouble() < physicalToSpecialRatio)
				MoveCategory.PHYSICAL
			else
				MoveCategory.SPECIAL
		}
		// Randomly selects the move from available options
		def randomMove(moveType: Type, category: MoveCategory) = {
			// TODO: Add handling of empty options
			val options = moves.byType(moveType) & moves.byCategory(category)
			val index = RandomSource.nextInt(options.size)
			options.iterator.drop(index).next()
		}
		// Generates a new move
		def newMove() = {
			// Prefers added type, if applicable
			// Secondarily, prefers own type
			// Thirdly, selects from related types
			val moveType = addedType.filter { _ => RandomSource.nextDouble() < addedTypeMoveChance }
				.getOrElse {
					if (RandomSource.nextDouble() < ownTypeMoveChance)
						currentTypes.random
					else
						currentRelations.random(typeWeights)
				}
			val category = randomCategoryIn(moveType)
			randomMove(moveType, category)
		}
		// Finds a relative random move
		def swapMove(original: Move) = {
			val moveType = {
				if (RandomSource.nextDouble() < sameTypeChance)
					original.`type`
				else if (RandomSource.nextDouble() < ownTypeMoveChance)
					currentTypes.random
				else
					TypeRelations.of(original.`type`).random(typeWeights)
			}
			val moveCategory = {
				if (RandomSource.nextDouble() < sameCategoryChance)
					original.category
				else
					randomCategoryIn(moveType)
			}
			// TODO: Find move with relative power
			randomMove(moveType, moveCategory)
		}
		
		/*
		boolean noBroken = settings.isBlockBrokenMovesetMoves();
				boolean forceStartingMoves = supportsFourStartingMoves() && settings.isStartWithGuaranteedMoves();
				int forceStartingMoveCount = settings.getGuaranteedMoveCount();
				double goodDamagingPercentage =
						settings.isMovesetsForceGoodDamaging() ? settings.getMovesetsGoodDamagingPercent() / 100.0 : 0;
				boolean evolutionMovesForAll = settings.isEvolutionMovesForAll();
		 */
		val originalEvoMoves = originalMovesLearnt.takeWhile { _.level == 0 }
		val originalStartingMoves = originalMovesLearnt.view
			.drop(originalEvoMoves.size).takeWhile { _.level == 1 }.toVector
		
		/*
		// 4 starting moves?
					if (forceStartingMoves) {
						int lv1count = 0;
						for (MoveLearnt ml : moves) {
							if (ml.level == 1) {
								lv1count++;
							}
						}
						if (lv1count < forceStartingMoveCount) {
							for (int i = 0; i < forceStartingMoveCount - lv1count; i++) {
								MoveLearnt fakeLv1 = new MoveLearnt();
								fakeLv1.level = 1;
								fakeLv1.move = 0;
								moves.add(0, fakeLv1);
							}
						}
					}
		
					if (evolutionMovesForAll) {
						if (moves.get(0).level != 0) {
							MoveLearnt fakeEvoMove = new MoveLearnt();
							fakeEvoMove.level = 0;
							fakeEvoMove.move = 0;
							moves.add(0, fakeEvoMove);
						}
					}
		 */
		/*
		if (pkmn.actuallyCosmetic) {
						for (int i = 0; i < moves.size(); i++) {
							moves.get(i).move = movesets.get(pkmn.baseForme.number).get(i).move;
						}
						continue;
					}
		 */
		/*
		// Find last lv1 move
					// lv1index ends up as the index of the first non-lv1 move
					int lv1index = moves.get(0).level == 1 ? 0 : 1; // Evolution move handling (level 0 = evo move)
					while (lv1index < moves.size() && moves.get(lv1index).level == 1) {
						lv1index++;
					}
		
					// last lv1 move is 1 before lv1index
					if (lv1index != 0) {
						lv1index--;
					}

		 */
	}
}
