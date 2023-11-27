package vf.controller

import com.dabomstew.pkrandom.Settings
import com.dabomstew.pkrandom.romhandlers.RomHandler
import vf.model.{EvolveGroup, Moves, Pokes}

/**
 * Tracks data collected during the randomization process
 * @author Mikko Hilpinen
 * @since 7.7.2023, v1.0-alt
 */
class JavaRandomizationContext(implicit romHandler: RomHandler, settings: Settings)
{
	// ATTRIBUTES   ---------------------
	
	private implicit val pokemons: Pokes = Pokes.all
	private implicit val evolveGroups: Set[EvolveGroup] = EvolveGroup.all
	private implicit val moves: Moves = Moves.inGame(romHandler)
	
	private var pokeMapping = Map[Int, Vector[EvolveGroup]]()
	private var minAppearanceLevels = Map[EvolveGroup, Int]().withDefaultValue(1)
	
	
	// OTHER    ------------------------
	
	def randomizeTypes() = {
		RandomizeTypes.all()
		RandomizeTypes.fixCosmeticForms()
	}
	def randomizeAbilities() = RandomizeAbilities.all(evolveGroups)
	def randomizeStats() = RandomizeStats.all()
	def makeEvolvesEasier() = MakeEvolvesEasier.all(evolveGroups)
	def randomizeMoves() = RandomizeMoves.all()
	def randomizeWildEncounters() = {
		val (mapping, appearance) = RandomizeWildEncounters.all(evolveGroups)
		pokeMapping = mapping
		minAppearanceLevels = appearance
	}
	def randomizeTrainerPokes() = RandomizeBattles.all(pokeMapping, minAppearanceLevels)
}
