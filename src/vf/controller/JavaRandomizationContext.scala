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
	
	private var typeConversions = Map[Int, Map[Type, Type]]()
	private var typeAdditions = Map[Int, Type]()
	
	private var statModifiers = Map[Int, Map[PokeStat, Double]]()
	private var statSwaps = Map[Int, Map[PokeStat, PokeStat]]()
	
	
	// OTHER    ------------------------
	
	def randomizeTypes() = {
		val (conversions, additions) = RandomizeTypes.all()
		RandomizeTypes.fixCosmeticForms()
		typeConversions = conversions
		typeAdditions = additions
	}
	def randomizeStats() = {
		val (mods, swaps) = RandomizeStats.all()
		statModifiers = mods
		statSwaps = swaps
	}
	def makeEvolvesEasier() = MakeEvolvesEasier.all()
	def randomizeMoves() = RandomizeMoves.all(typeConversions, typeAdditions, statModifiers, statSwaps)
}
