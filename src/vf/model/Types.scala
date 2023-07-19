package vf.model

import com.dabomstew.pkrandom.pokemon.Pokemon

import java.util
import scala.jdk.CollectionConverters._

object Types
{
	// OTHER    -----------------------
	
	def all(implicit pokemons: Pokemons) =
		apply(pokemons.iterator.map { p => p.number -> TypeSet.from(p) }.toMap)
	
	def from(pokemon: util.List[Pokemon]) = {
		apply(pokemon.iterator().asScala.flatMap { Option(_) }.map { p => p.number -> TypeSet.from(p) }.toMap)
	}
}

/**
 * Contains type information about a set of pokemon
 * @author Mikko Hilpinen
 * @since 3.7.2023, v1.0-alt
 */
case class Types(values: Map[Int, TypeSet])
{
	def apply(mon: Pokemon) = values(mon.number)
}