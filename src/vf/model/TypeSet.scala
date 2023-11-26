package vf.model

import com.dabomstew.pkrandom.RandomSource
import com.dabomstew.pkrandom.pokemon.{Pokemon, Type}
import com.dabomstew.pkrandom.romhandlers.RomHandler
import utopia.flow.collection.immutable.Pair

object TypeSet
{
	type PokeType = Type
	
	def from(pokemon: Pokemon) =
		apply(pokemon.primaryType, Option(pokemon.secondaryType).filterNot { _ == pokemon.primaryType })
	
	def apply(primary: Type, secondary: Type): TypeSet =
		new TypeSet(primary, Some(secondary).filterNot { _ == primary })
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
	
	def random = secondary.filter { _ => RandomSource.nextBoolean() }.getOrElse(primary)
	
	def isSingleType = secondary.isEmpty
	def isDualType = !isSingleType
	
	def relations(implicit rom: RomHandler) = TypeRelations.of(this)
	
	
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
	
	def withPrimary(t: Type) = copy(primary = t)
	def withSecondary(t: Type) = copy(secondary = Some(t))
}