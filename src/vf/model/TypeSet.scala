package vf.model

import com.dabomstew.pkrandom.RandomSource
import com.dabomstew.pkrandom.pokemon.Pokemon
import com.dabomstew.pkrandom.romhandlers.RomHandler
import utopia.flow.collection.immutable.Pair

object TypeSet
{
	def from(pokemon: Pokemon) = {
		val primary = PokeType.fromJava(pokemon.primaryType)
		apply(primary, Option(pokemon.secondaryType).map(PokeType.fromJava).filterNot { _ == primary })
	}
	
	def apply(primary: PokeType, secondary: PokeType): TypeSet =
		new TypeSet(primary, Some(secondary).filterNot { _ == primary })
}

/**
 * Represents a combination of 1-2 types
 * @author Mikko Hilpinen
 * @since 3.7.2023, v1.0-alt
 */
case class TypeSet(primary: PokeType, secondary: Option[PokeType] = None)
{
	// COMPUTED ----------------------
	
	def types = secondary match {
		case Some(s) => Pair(primary, s)
		case None => Vector(primary)
	}
	
	def random = secondary.filter { _ => RandomSource.nextBoolean() }.getOrElse(primary)
	
	def isSingleType = secondary.isEmpty
	def isDualType = !isSingleType
	
	def effectiveness = EffectivenessRelations(this)
	def relations(implicit rom: RomHandler) = TypeRelations.of(this)
	
	
	// IMPLEMENTED  -----------------
	
	override def toString = secondary match {
		case Some(secondary) => s"$primary|$secondary"
		case None => primary.toString
	}
	
	
	// OTHER    ---------------------
	
	def contains(t: PokeType) = primary == t || secondary.contains(t)
	
	def &&(other: TypeSet) = {
		if (contains(other.primary)) {
			if (other.secondary.exists(contains))
				Some(this)
			else
				Some(TypeSet(other.primary))
		}
		else
			other.secondary.filter(contains).map { TypeSet(_) }
	}
	
	def withPrimary(t: PokeType) = copy(primary = t)
	def withSecondary(t: PokeType) = copy(secondary = Some(t))
}