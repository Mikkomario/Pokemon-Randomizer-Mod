package vf.model

import com.dabomstew.pkrandom.pokemon.Pokemon
import com.dabomstew.pkrandom.romhandlers.RomHandler
import utopia.flow.collection.CollectionExtensions._
import utopia.flow.collection.immutable.caching.cache.Cache

import scala.jdk.CollectionConverters._

object Pokes
{
	def all(implicit rom: RomHandler) = {
		new Pokes(rom.getPokemonInclFormes.iterator().asScala.flatMap { Option(_) }.toVector.groupBy { _.number }
			.map { case (number, forms) =>
				val pokeForms: Vector[Poke] = forms.oneOrMany match {
					case Left(onlyForm) => Vector(new Poke(onlyForm))
					case Right(forms) =>
						forms.groupBy { _.baseForme }.flatMap { case (base, forms) =>
							forms.oneOrMany match {
								case Left(onlyForm) => Iterable.single(new Poke(onlyForm))
								case Right(forms) =>
									val (altForms, cosmeticForms) = forms.divideBy { _.actuallyCosmetic }.toTuple
									Option(base) match {
										case Some(base) => new Poke(base, cosmeticForms) +: altForms.map { new Poke(_) }
										case None =>
											if (cosmeticForms.isEmpty)
												altForms.map { new Poke(_) }
											else if (altForms.isEmpty) {
												val primary = cosmeticForms.random
												Iterable.single(new Poke(primary, cosmeticForms.filterNot { _ == primary }))
											}
											else {
												val primary = altForms.random
												new Poke(primary, cosmeticForms) +:
													altForms.filterNot { _ == primary }.map { new Poke(_) }
											}
									}
							}
						}.toVector
				}
				number -> pokeForms
			})
	}
}

/**
 * Contains (mutable) information about pokes during randomization
 * @author Mikko Hilpinen
 * @since 3.7.2023, v1.0-alt
 */
class Pokes(values: Map[Int, Vector[Poke]])(implicit rom: RomHandler) extends Iterable[Poke]
{
	// ATTRIBUTES   ----------------------------
	
	private val wrappersCache = Cache { p: Pokemon =>
		values.get(p.number).flatMap { _.find { _.represents(p) } }.getOrElse { new Poke(p) }
	}
	
	/**
	 * Pokes that appear only as cosmetic forms
	 */
	lazy val cosmeticForms = iterator.flatMap { _.cosmeticForms }
	
	
	// OTHER    --------------------------------
	
	/**
	 * @param pokeNum Number of the targeted poke
	 * @return Pokemon matching that number. None if not found.
	 */
	def apply(pokeNum: Int) = values.getOrElse(pokeNum, Vector.empty)
	/**
	 * @param poke A pokemon to wrap
	 * @return Poke wrapper for that pokemon
	 */
	def apply(poke: Pokemon) = wrappersCache(poke)
	
	
	// IMPLEMENTED  ---------------------------
	
	override def iterator: Iterator[Poke] = values.valuesIterator.flatten
}