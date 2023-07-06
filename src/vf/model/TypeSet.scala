package vf.model

import com.dabomstew.pkrandom.pokemon.{Pokemon, Type}
import utopia.flow.collection.immutable.Pair

object TypeSet
{
	def from(pokemon: Pokemon) = apply(pokemon.primaryType, Option(pokemon.secondaryType))
	
	def apply(primary: Type, secondary: Type): TypeSet = new TypeSet(primary, Some(secondary))
}

/**
 * Represents a combination of 1-2 types
 * @author Mikko Hilpinen
 * @since 3.7.2023, v1.0-alt
 */
case class TypeSet(primary: Type, secondary: Option[Type] = None)
{
	// COMPUTED ----------------------
	
	def types = secondary match {
		case Some(s) => Pair(primary, s)
		case None => Vector(primary)
	}
	
	
	// OTHER    ---------------------
	
	def contains(t: Type) = primary == t || secondary.contains(t)
	
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
}