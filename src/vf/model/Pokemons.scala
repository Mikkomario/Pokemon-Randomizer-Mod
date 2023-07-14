package vf.model

import com.dabomstew.pkrandom.pokemon.Pokemon

import scala.jdk.CollectionConverters._

object Pokemons
{
	def from(mons: java.util.List[Pokemon]) =
		apply(mons.iterator().asScala.flatMap { Option(_) }.map { p => p.number -> p }.toMap)
}

/**
 * Contains (mutable) information about pokes during randomization
 * @author Mikko Hilpinen
 * @since 3.7.2023, v1.0-alt
 */
case class Pokemons(values: Map[Int, Pokemon]) extends Iterable[Pokemon]
{
	// ATTRIBUTES   ----------------------------
	
	/**
	 * Pokemons that appear only as cosmetic forms
	 */
	lazy val cosmeticForms = iterator.filter { _.actuallyCosmetic }.toVector
	
	
	// OTHER    --------------------------------
	
	/**
	 * @param pokeNum Number of the targeted poke
	 * @return Pokemon matching that number. None if not found.
	 */
	def apply(pokeNum: Int) = values.get(pokeNum)
	
	
	// IMPLEMENTED  ---------------------------
	
	override def iterator: Iterator[Pokemon] = values.valuesIterator
}