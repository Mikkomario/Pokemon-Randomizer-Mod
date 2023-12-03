package vf.controller

import com.dabomstew.pkrandom.pokemon.{CriticalChance, MoveCategory, Pokemon}
import com.dabomstew.pkrandom.romhandlers.RomHandler
import utopia.flow.collection.CollectionExtensions._
import utopia.vault.database.Connection
import vf.model._
import vf.poke.core.database.access.many.game.ability.DbAbilities
import vf.poke.core.database.access.many.game.item.DbItems
import vf.poke.core.database.access.single.game.DbGame
import vf.poke.core.database.model.game.{AbilityModel, GameModel, ItemModel}
import vf.poke.core.database.model.poke._
import vf.poke.core.database.model.randomization._
import vf.poke.core.model.enumeration.CriticalRate.{Guaranteed, Increased, Normal}
import vf.poke.core.model.enumeration.MoveCategory.{Physical, Special}
import vf.poke.core.model.partial.game.{AbilityData, GameData, ItemData}
import vf.poke.core.model.partial.poke._
import vf.poke.core.model.partial.randomization._
import vf.util.PokeExtensions._

object RecordToDb
{
	/**
	 * @param connection Implicit DB Connection
	 * @param rom Implicit ROM to use
	 * @return An interface for the unmodified game instance
	 */
	def original(implicit connection: Connection, rom: RomHandler) = {
		// Inserts the game to the database, if appropriate
		val game = this.game
		val gameId = game.either.id
		// Inserts the unmodified randomization instance, if not found already
		val randomizationId = game.toOption.flatMap { _.access.unmodified.id }.getOrElse {
			RandomizationModel.insert(RandomizationData(gameId, isOriginal = true)).id
		}
		// TODO: Insert the original game data
		new RecordToDb(gameId, randomizationId)
	}
	
	private def game(implicit connection: Connection, rom: RomHandler) =
		DbGame.withName(rom.getROMName).pull.toRight { GameModel.insert(GameData(rom.getROMName)) }
	
	/**
	 * Inserts a new randomization instance and returns an interface for it
	 * @param connection Implicit DB Connection
	 * @param rom Implicit ROM to use
	 * @return New randomization recording interface
	 */
	def newRandomization()(implicit connection: Connection, rom: RomHandler) = {
		val gameId = game.either.id
		new RecordToDb(gameId, RandomizationModel.insert(RandomizationData(gameId)).id)
	}
}

/**
 * An interface used for recording game data into the database
 * @author Mikko Hilpinen
 * @since 1.12.2023, v1.0-alt
 */
class RecordToDb(gameId: Int, randomizationId: Int)(implicit rom: RomHandler)
{
	// ATTRIBUTES   -----------------------
	
	// Keys are poke numbers + forme numbers, values are db ids
	private var pokeMap = Map[(Int, Int), Int]()
	
	
	// OTHER    ---------------------------
	
	// NB: At this time, won't check for duplicates
	def pokes()(implicit pokes: Pokes, connection: Connection) = {
		pokeMap = PokeModel
			.insert(pokes.iterator.map { poke =>
				PokeData(randomizationId, poke.number, if (poke.wrapped.actuallyCosmetic) 0 else poke.wrapped.formeNumber,
					poke.name, poke.primaryType, poke.secondaryType)
			}.toVector)
			.map { poke => (poke.number, poke.formeIndex) -> poke.id }.toMap
	}
	
	def evolves()(implicit pokes: Pokes, connection: Connection) = {
		val evos = pokes.flatMap { poke =>
			lazy val pokeId = idOf(poke)
			poke.toEvos.map { pokeId -> _ }
		}
		// Inserts the items first
		val itemIndices = evos.flatMap { case (_, evo) => evo.item }.toSet
		val itemIdMap = {
			if (itemIndices.nonEmpty) {
				val recordedItems = DbItems.inGame(gameId).withInGameIds(itemIndices).pull
				val recordedItemIndices = recordedItems.map { _.indexInGame }.toSet
				(recordedItems ++ ItemModel.insert(itemIndices.diff(recordedItemIndices).toVector
					.map { index => ItemData(gameId, index, rom.getItemNames.lift(index).getOrElse("")) }))
					.map { item => item.indexInGame -> item.id }.toMap
			}
			else
				Map[Int, Int]()
		}
		// Then inserts the evos
		EvoModel.insert(evos.map { case (pokeId, evo) =>
			EvoData(pokeId, idOf(evo.wrapped.to), evo.levelThreshold, evo.item.flatMap(itemIdMap.get))
		}.toVector)
	}
	
	def abilities()(implicit pokes: Pokes, connection: Connection) = {
		// [Poke id, ability index, hidden]
		val abilityData = pokes.iterator.flatMap { poke =>
			lazy val pokeId = idOf(poke)
			poke.abilities.map { case (abilityIndex, isHidden) => (pokeId, abilityIndex, isHidden) }
		}
		// Records the abilities first
		val abilityIndices = abilityData.map { _._2 }.toSet
		val recordedAbilities = DbAbilities.inGame(gameId).withInGameIds(abilityIndices).pull
		val recordedAbilityIndices = recordedAbilities.map { _.indexInGame }.toSet
		val abilityIdMap = (recordedAbilities ++ AbilityModel.insert(abilityIndices.diff(recordedAbilityIndices).toVector
			.map { index => AbilityData(gameId, index, rom.abilityName(index)) }))
			.map { a => a.indexInGame -> a.id }
			.toMap
		// Then which ability belongs to which poke
		PokeAbilityModel.insert(abilityData.map { case (pokeId, abilityIndex, isHidden) =>
			PokeAbilityData(pokeId, abilityIdMap(abilityIndex), isHidden = isHidden)
		}.toVector)
	}
	
	def stats()(implicit pokes: Pokes, connection: Connection) =
		PokeStatModel.insert(pokes.flatMap { p =>
			p.stats.map { case (stat, value) => PokeStatData(idOf(p), stat, value) } }.toVector)
	
	def moves(movesPerPoke: Map[Poke, Vector[MoveLearn]])(implicit moves: Moves, connection: Connection) = {
		// Inserts the move information
		val moveIdMap = MoveModel.insert(movesPerPoke.valuesIterator.flatMap { _.map { _.move } }.toSet
			.map { moveIndex: Int =>
				val move = moves.byNumber(moveIndex)
				val category = move.category match {
					case MoveCategory.PHYSICAL => Some(Physical)
					case MoveCategory.SPECIAL => Some(Special)
					case _ => None
				}
				val criticalRate = move.criticalChance match {
					case CriticalChance.INCREASED => Increased
					case CriticalChance.GUARANTEED => Guaranteed
					case _ => Normal
				}
				MoveData(randomizationId, moveIndex, move.name, move.`type`, category, move.power, move.hitCount,
					move.hitratio, criticalRate)
			}.toVector)
			.map { m => m.indexInGame -> m.id }.toMap
		// Next inserts the move learn events
		val (levelMoveData, evoMoveData) = movesPerPoke.splitFlatMap { case (poke, moveLearns) =>
			val pokeId = idOf(poke)
			val (levelMoves, evoMoves) = moveLearns.divideBy { _.level == 0 }.toTuple
			val levelMoveData = levelMoves
				.map { moveLearn => MoveLearnData(pokeId, moveIdMap(moveLearn.move), moveLearn.level) }
			val evoMoveData = evoMoves.map { moveLearn => EvoMoveData(pokeId, moveLearn.move) }
			levelMoveData -> evoMoveData
		}
		MoveLearnModel.insert(levelMoveData)
		EvoMoveModel.insert(evoMoveData)
	}
	
	def starters(starterTrios: Vector[Vector[Poke]])(implicit connection: Connection) = {
		val setIds = StarterSetModel.insert(starterTrios.indices.map { i => StarterSetData(randomizationId, i) })
			.map { _.id }
		StarterAssignmentModel.insert(starterTrios.zip(setIds).flatMap { case (pokes, setId) =>
			pokes.zipWithIndex.map { case (poke, index) => StarterAssignmentData(setId, idOf(poke), index) }
		})
	}
	
	def wildEncounters(encounters: Iterable[WildEncounter])(implicit connection: Connection) = {
		WildEncounterModel.insert(encounters.groupBy { _.poke }.flatMap { case (poke, encounters) =>
			val pokeId = idOf(poke)
			encounters.groupBy { _.zone }.flatMap { case (zoneIndex, encounters) =>
				encounters.groupBy { _.levelRange }.map { case (levelRange, encounters) =>
					WildEncounterData(randomizationId, zoneIndex, pokeId, levelRange, encounters.size)
				}
			}
		}.toVector)
	}
	
	def battles(encounterCounts: Map[Poke, Map[Int, Int]])(implicit connection: Connection) = {
		BattleEncounterModel.insert(encounterCounts.flatMap { case (poke, encounterCounts) =>
			val pokeId = idOf(poke)
			encounterCounts.map { case (level, count) => BattleEncounterData(randomizationId, pokeId, level, count) }
		}.toVector)
	}
	
	def idOf(poke: Pokemon) = pokeMap(poke.number -> (if (poke.actuallyCosmetic) 0 else poke.formeNumber))
	def idOf(poke: Poke): Int = idOf(poke.wrapped)
}
