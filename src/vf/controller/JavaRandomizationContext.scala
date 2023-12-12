package vf.controller

import com.dabomstew.pkrandom.Settings
import com.dabomstew.pkrandom.pokemon.{Evolution, Pokemon}
import com.dabomstew.pkrandom.romhandlers.RomHandler
import utopia.flow.collection.CollectionExtensions._
import utopia.flow.parse.file.FileExtensions._
import utopia.flow.parse.json.{JsonParser, JsonReader}
import utopia.flow.util.TryCatch
import utopia.vault.database.columnlength.ColumnLengthRules
import vf.model._
import vf.poke.core.util.Common._

import java.nio.file.Paths

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
	
	private var movesByPoke = Map[Poke, Vector[MoveLearn]]()
	
	private var starters = Vector[Vector[EvolveGroup]]()
	private var wildEncounters = Vector[WildEncounter]()
	private var encounterCounts = Map[Poke, Map[Int, Int]]()
	
	
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
	}
	def randomizeAbilities() = RandomizeAbilities.all(evolveGroups)
	def randomizeStats() = {
		// Applies the eviolite stat changes before the randomization, so that they make more sense in the game's context
		ProcessEviolites.stats(eviolites, groupByNumber)
		RandomizeStats.all(evolveGroups)
		// Also applies type stat "compensation" in order to make the pokes more characteristic of the new type
		ReflectTypeChangesInStats()
	}
	def makeEvolvesEasier() = MakeEvolvesEasier.all(evolveGroups)
	def randomizeStarters() = {
		val (mapping, trios) = RandomizeStarters(evolveGroups, groupByNumber)
		starterMapping = mapping
		starters = trios
	}
	def randomizeWildEncounters() = {
		val (mapping, appearance, encounters) = RandomizeWildEncounters.all(evolveGroups)
		pokeMapping = mapping
		minAppearanceLevels = appearance
		wildEncounters = encounters
	}
	def randomizeMoves() = movesByPoke = RandomizeMoves.all(evolveGroups, minAppearanceLevels)
	def randomizeTrainerPokes() = {
		encounterCounts = RandomizeBattles.all(groupByNumber, starterMapping, pokeMapping, minAppearanceLevels)
		// Also balances pokes afterwards based on their types
		CompensateForType(minAppearanceLevels ++ starterMapping.valuesIterator.map { _ -> 5 }, encounterCounts)
	}
	
	def record() = {
		cPool.tryWith { implicit c =>
			println("Applying length rules")
			implicit val jsonParser: JsonParser = JsonReader
			Paths.get("data")
				.tryIterateChildrenCatching {
					_.filter { f => f.fileName.contains("length") && f.fileType == "json" }
						.map { ColumnLengthRules.loadFrom(_) }
						.toTryCatch
				} match
			{
				case TryCatch.Success(_, failures) =>
					failures.headOption.foreach { log(_, s"Failed to apply ${ failures.size } length rules") }
				case TryCatch.Failure(error) => log(error, "Couldn't apply length rules")
			}
			println("Recording results to the database")
			val record = RecordToDb.newRandomization()
			record.pokes()
			record.evolves()
			record.abilities()
			record.stats()
			record.moves(movesByPoke)
			record.starters(starters.map { _.map { _.firstForm } })
			record.wildEncounters(wildEncounters)
			record.battles(encounterCounts)
			println("Results recorded")
		}.logFailure
	}
}
