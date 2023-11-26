package vf.model

import com.dabomstew.pkrandom.pokemon.{MegaEvolution, Pokemon}
import com.dabomstew.pkrandom.romhandlers.RomHandler
import utopia.flow.collection.immutable.Pair

import scala.jdk.CollectionConverters._

object PokeState
{
	def from(poke: Pokemon)(implicit rom: RomHandler) = apply(
		types = TypeSet.from(poke),
		stats = PokeStat.values.map { s => s -> s.of(poke) }.toMap,
		evos = Pair(poke.evolutionsFrom, poke.evolutionsTo).map { _.asScala.toVector.map(EvoState.from) },
		megaEvos = Pair(poke.megaEvolutionsFrom, poke.megaEvolutionsTo).map { _.asScala.toVector },
		moves = Option(rom.getMovesLearnt.get(poke.number)) match {
			case Some(moves) => moves.asScala.map(MoveLearn.from).toVector.sortBy { _.level }
			case None => Vector()
		}
	)
}

/**
 * An immutable model for representing the state of a single poke at some point during the randomization process
 * @author Mikko Hilpinen
 * @since 25.11.2023, v0.1
 * @param evos Evolutions of this poke. First the 'from' evos (i.e. from previous forms)
 *             and second the 'to' evos (i.e. next forms)
 */
case class PokeState(types: TypeSet, stats: Map[PokeStat, Int], evos: Pair[Vector[EvoState]],
                     megaEvos: Pair[Vector[MegaEvolution]], moves: Vector[MoveLearn])
	extends PokeLike[EvoState]
{
	// OTHER    -----------------------
	
	def withTypes(types: TypeSet) = copy(types = types)
	def mapTypes(f: TypeSet => TypeSet) = withTypes(f(types))
}