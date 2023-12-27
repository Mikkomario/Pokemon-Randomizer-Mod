package vf.model

import com.dabomstew.pkrandom.pokemon.{MegaEvolution, Pokemon}
import com.dabomstew.pkrandom.romhandlers.RomHandler
import utopia.flow.collection.immutable.Pair
import vf.poke.core.model.cached.TypeSet
import vf.poke.core.model.enumeration.Stat
import vf.util.PokeExtensions._

import scala.jdk.CollectionConverters._

object PokeState
{
	def from(poke: Pokemon)(implicit rom: RomHandler) = apply(
		types = poke.types,
		abilities = Vector(poke.ability1 -> false, poke.ability2 -> false, poke.ability3 -> true)
			.filterNot { _._1 == 0 }.distinct,
		stats = Stat.values.map { s => s -> poke(s) }.toMap,
		evos = Pair(poke.evolutionsTo, poke.evolutionsFrom).map { _.asScala.toVector.map(EvoState.from) },
		megaEvos = Pair(poke.megaEvolutionsTo, poke.megaEvolutionsFrom).map { _.asScala.toVector },
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
case class PokeState(types: TypeSet, abilities: Vector[(Int, Boolean)], stats: Map[Stat, Int],
                     evos: Pair[Vector[EvoState]], megaEvos: Pair[Vector[MegaEvolution]], moves: Vector[MoveLearn])
	extends PokeLike[EvoState]
{
	// OTHER    -----------------------
	
	def withTypes(types: TypeSet) = copy(types = types)
	def mapTypes(f: TypeSet => TypeSet) = withTypes(f(types))
}