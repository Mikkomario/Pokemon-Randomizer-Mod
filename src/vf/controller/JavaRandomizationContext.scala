package vf.controller

import com.dabomstew.pkrandom.Settings
import com.dabomstew.pkrandom.pokemon.{Evolution, Pokemon}
import com.dabomstew.pkrandom.romhandlers.RomHandler
import vf.model.{EvolveGroup, Moves, Pokes}

object JavaRandomizationContext
{
	def apply(romHandler: RomHandler, settings: Settings) = {
		implicit val rom: RomHandler = romHandler
		implicit val s: Settings = settings
		// The eviolites need to be processed before any other operation is initiated
		val eviolites = ProcessEviolites.preprocess()
		new JavaRandomizationContext(eviolites)
	}
}

/**
 * Tracks data collected during the randomization process
 * @author Mikko Hilpinen
 * @since 7.7.2023, v1.0-alt
 */
class JavaRandomizationContext(eviolites: Map[Pokemon, Vector[Evolution]])
                              (implicit romHandler: RomHandler, settings: Settings)
{
	// ATTRIBUTES   ---------------------
	
	private implicit val pokemons: Pokes = Pokes.all
	private implicit val moves: Moves = Moves.inGame(romHandler)
	
	private val evolveGroups: Vector[EvolveGroup] = EvolveGroup.all
	private val groupByNumber = evolveGroups.flatMap { g => g.iterator.map { _.number -> g } }.toMap
	
	private var starterMapping = Map[EvolveGroup, EvolveGroup]()
	private var pokeMapping = Map[Int, Vector[EvolveGroup]]()
	private var minAppearanceLevels = Map[EvolveGroup, Int]().withDefaultValue(1)
	
	
	// INITIAL CODE ---------------------
	
	Log("evolve-groups") { writer =>
		evolveGroups.sortBy { _.forms.head.number }.foreach { group =>
			writer.println(s"$group ${group.types.mkString("|")}:")
			group.iterator.foreach { poke =>
				writer.println(s"\t- ${poke.name}")
			}
		}
	}
	
	
	// OTHER    ------------------------
	
	def randomizeTypes() = {
		RandomizeTypes.all(evolveGroups)
		RandomizeTypes.fixCosmeticForms()
		// Also applies type stat "compensation" in order to make the pokes more characteristic of the new type
		ReflectTypeChangesInStats()
	}
	def randomizeAbilities() = RandomizeAbilities.all(evolveGroups)
	def randomizeStats() = {
		// Applies the eviolite stat changes before the randomization, so that they make more sense in the game's context
		ProcessEviolites.stats(eviolites, groupByNumber)
		RandomizeStats.all(evolveGroups)
	}
	def makeEvolvesEasier() = MakeEvolvesEasier.all(evolveGroups)
	def randomizeStarters() = starterMapping = RandomizeStarters(evolveGroups, groupByNumber)
	def randomizeWildEncounters() = {
		val (mapping, appearance) = RandomizeWildEncounters.all(evolveGroups)
		pokeMapping = mapping
		minAppearanceLevels = appearance
	}
	def randomizeMoves() = RandomizeMoves.all(evolveGroups, minAppearanceLevels)
	def randomizeTrainerPokes() = {
		val encounterCounts = RandomizeBattles.all(groupByNumber, starterMapping, pokeMapping, minAppearanceLevels)
		// Also balances pokes afterwards based on their types
		val pokePool = minAppearanceLevels.keySet.flatMap { _.iterator } ++ encounterCounts.keySet ++
			starterMapping.valuesIterator.flatMap { _.iterator }
		CompensateForType(pokePool, encounterCounts)
	}
}
