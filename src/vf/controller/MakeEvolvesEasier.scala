package vf.controller

import com.dabomstew.pkrandom.constants.Items
import com.dabomstew.pkrandom.pokemon.EvolutionType
import utopia.flow.collection.CollectionExtensions._
import vf.model.{EvolveGroup, Poke}

/**
 * Replaces inconvenient evolutions with easier evolutions.
 * Intended to be called after other methods to make evolves easier.
 * @author Mikko Hilpinen
 * @since 8.7.2023, v1.0-alt
 */
object MakeEvolvesEasier
{
	// ATTRIBUTES   ------------------------
	
	private val level = EvolutionType.LEVEL
	
	private val firstEvolveLevel = 21
	private val lastEvolveLevel = 36
	private val midFormLevels = 15
	
	// Prioritized list of evolution types which to convert into "level"
	// Applied on pokes that have splitting evolves
	private val toLevelOptions = Vector(
		EvolutionType.LEVEL_HIGH_BEAUTY,
		EvolutionType.LEVEL_UPSIDE_DOWN,
		EvolutionType.HAPPINESS_NIGHT,
		EvolutionType.LEVEL_NIGHT_GAME,
		EvolutionType.LEVEL_ITEM_NIGHT,
		EvolutionType.LEVEL_DUSK,
		EvolutionType.LEVEL_NIGHT,
		EvolutionType.LEVEL_RAIN,
		EvolutionType.LEVEL_SNOWY,
		EvolutionType.HAPPINESS_DAY,
		EvolutionType.HAPPINESS,
		EvolutionType.LEVEL_DAY_GAME,
		EvolutionType.LEVEL_ITEM_DAY,
		EvolutionType.LEVEL_DAY,
		EvolutionType.LEVEL_GAME,
		EvolutionType.LEVEL_ICY_ROCK,
		EvolutionType.LEVEL_MOSS_ROCK,
		EvolutionType.LEVEL_ELECTRIFIED_AREA
	)
	
	
	// OTHER    ----------------------------
	
	def all(evoGroups: IterableOnce[EvolveGroup]) = evoGroups.iterator.foreach { group =>
		group.iterator.foreach(apply)
	}
	
	def apply(poke: Poke) = {
		val evolves = poke.toEvos
		lazy val evolveLevel = {
			if (poke.isBasicForm)
				firstEvolveLevel
			else
				poke.fromEvos.findMap { _.levelThreshold } match {
					case Some(earlier) => earlier + midFormLevels
					case None => lastEvolveLevel
				}
		}
		
		if (evolves.nonEmpty) {
			evolves.oneOrMany match {
				case Left(onlyEvolve) =>
					if (onlyEvolve.nonLevelBased)
						onlyEvolve.makeLevelBased(evolveLevel)
				case Right(evolves) =>
					// Converts one of the evolves into a simple level evolve, if possible
					val hasLevel = evolves.exists { _.evoType == level }
					val toLevelEvo =
						if (hasLevel) None else toLevelOptions.findMap { o => evolves.find { _.evoType == o } }
					toLevelEvo.foreach { _.makeLevelBased(evolveLevel) }
					// May modify the other evolves
					evolves.iterator.foreach { evo =>
						// Makes to evolve with a stone
						def convertToStone(stone: Int) = {
							if (!evolves.exists { _.usesStone(stone) }) {
								evo.makeStoneBased(stone)
								// Also gives that stone as held item
								poke.giveItem(stone)
							}
						}
						evo.evoType match {
							// Case: Item-related evo => Gives the required item as a held item
							case EvolutionType.STONE | EvolutionType.STONE_MALE_ONLY | EvolutionType.STONE_FEMALE_ONLY |
							     EvolutionType.LEVEL_ITEM_DAY |
							     EvolutionType.LEVEL_ITEM_NIGHT => poke.giveItem(evo.extraInfo)
							// Case: Area-related evo => Converts to a stone-evo
							case EvolutionType.LEVEL_ELECTRIFIED_AREA => convertToStone(Items.thunderStone)
							case EvolutionType.LEVEL_MOSS_ROCK => convertToStone(Items.leafStone)
							case EvolutionType.LEVEL_ICY_ROCK => convertToStone(Items.waterStone)
							case _ => ()
						}
					}
			}
			poke.updateState()
		}
	}
}
