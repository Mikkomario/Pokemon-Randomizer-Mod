package vf.controller

import com.dabomstew.pkrandom.constants.Items
import com.dabomstew.pkrandom.pokemon.{EvolutionType, Pokemon}
import com.dabomstew.pkrandom.romhandlers.RomHandler
import utopia.flow.collection.CollectionExtensions._
import vf.util.PokemonExtensions._

import scala.jdk.CollectionConverters._

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
	private val stones = Set(EvolutionType.STONE, EvolutionType.STONE_MALE_ONLY, EvolutionType.STONE_FEMALE_ONLY)
	
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
	
	// TODO: Ordering is important. This implementation assumes that non-evolved
	//  forms appear before the evolved forms in the input list
	def all()(implicit rom: RomHandler) = rom.getPokemonInclFormes.forEach { poke => Option(poke).foreach(apply) }
	
	def apply(poke: Pokemon) = {
		val evolves = poke.evolutionsTo.iterator().asScala.toVector
		lazy val evolveLevel = {
			if (poke.evolutionsFrom.isEmpty)
				firstEvolveLevel
			else
				poke.evolutionsFrom.iterator().asScala.find { _.`type`.usesLevel() } match {
					case Some(earlier) => earlier.extraInfo + midFormLevels
					case None => lastEvolveLevel
				}
		}
		
		if (evolves.nonEmpty) {
			evolves.oneOrMany match {
				case Left(onlyEvolve) =>
					if (onlyEvolve.`type` != level) {
						onlyEvolve.`type` = level
						onlyEvolve.extraInfo = evolveLevel
					}
				case Right(evolves) =>
					// Converts one of the evolves into a simple level evolve, if possible
					val hasLevel = evolves.exists { _.`type` == level }
					val toLevelEvo = {
						if (hasLevel)
							None
						else
							toLevelOptions.findMap { o => evolves.find { _.`type` == o } }
					}
					toLevelEvo.foreach { evo =>
						evo.`type` = level
						evo.extraInfo = evolveLevel
					}
					// May modify the other evolves
					evolves.iterator.foreach { evo =>
						// Makes to evolve with a stone
						def convertToStone(stone: Int) = {
							if (!evolves.exists { e => stones.contains(e.`type`) && e.extraInfo == stone }) {
								evo.`type` = EvolutionType.STONE
								evo.extraInfo = stone
								// Also gives that stone as held item
								poke.giveItem(stone)
							}
						}
						evo.`type` match {
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
		}
	}
}
