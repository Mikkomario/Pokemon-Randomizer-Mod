package vf.controller

import com.dabomstew.pkrandom.Settings
import com.dabomstew.pkrandom.pokemon.Type
import com.dabomstew.pkrandom.romhandlers.RomHandler
import vf.model.{EvolveGroup, Moves, PokeStat, Pokemons, Types}

/**
 * Tracks data collected during the randomization process
 * @author Mikko Hilpinen
 * @since 7.7.2023, v1.0-alt
 */
class JavaRandomizationContext(implicit romHandler: RomHandler, settings: Settings)
{
	// ATTRIBUTES   ---------------------
	
	private implicit val pokemons: Pokemons = Pokemons.from(romHandler.getPokemonInclFormes)
	private implicit val originalTypes: Types = Types.all
	private implicit val evolveGroups: Set[EvolveGroup] = EvolveGroup.all
	private implicit val moves: Moves = Moves.inGame(romHandler)
	
	private val originalBstValues = pokemons.iterator.map { p => p.number -> p.bstForPowerLevels() }.toMap
	private var modifiedBstValues = originalBstValues
	
	private var typeConversions = Map[Int, Map[Type, Type]]()
	private var typeAdditions = Map[Int, Type]()
	private var modifiedTypes = originalTypes
	
	private var statModifiers = Map[Int, Map[PokeStat, Double]]()
	private var statSwaps = Map[Int, Map[PokeStat, PokeStat]]()
	
	private var pokeMapping = Map[Int, Vector[EvolveGroup]]()
	private var minAppearanceLevels = Map[EvolveGroup, Int]().withDefaultValue(1)
	
	
	// OTHER    ------------------------
	
	def randomizeTypes() = {
		val (conversions, additions) = RandomizeTypes.all()
		RandomizeTypes.fixCosmeticForms()
		typeConversions = conversions
		typeAdditions = additions
		modifiedTypes = Types.all
	}
	def randomizeStats() = {
		val (mods, swaps) = RandomizeStats.all()
		statModifiers = mods
		statSwaps = swaps
		modifiedBstValues = pokemons.iterator.map { p => p.number -> p.bstForPowerLevels() }.toMap
	}
	def makeEvolvesEasier() = MakeEvolvesEasier.all()
	def randomizeMoves() = RandomizeMoves
		.all(modifiedTypes, typeConversions, typeAdditions, statModifiers, statSwaps)
	def randomizeWildEncounters() = {
		val (mapping, appearance) = RandomizeWildEncounters.all(evolveGroups, originalBstValues, modifiedBstValues,
			originalTypes, modifiedTypes)
		pokeMapping = mapping
		minAppearanceLevels = appearance
	}
}
