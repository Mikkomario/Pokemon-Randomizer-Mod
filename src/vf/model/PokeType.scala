package vf.model

import com.dabomstew.pkrandom.pokemon.Type

import scala.language.implicitConversions

/**
 * An enumeration of represents poke types in the game
 * @author Mikko Hilpinen
 * @since 29.11.2023, v1.0-alt
 */
sealed trait PokeType
{
	def toJava: Type
}

object PokeType
{
	// ATTRIBUTES   -----------------------
	
	val values = Vector[PokeType](Normal, Fighting, Flying, Grass, Water, Fire, Rock, Ground, Psychic, Bug, Dragon,
		Electric, Ghost, Poison, Ice, Steel, Dark, Fairy)
	val map = values.map { t => t.toJava -> t }.toMap
	
	
	// IMPLICIT ---------------------------
	
	implicit def fromJava(t: Type): PokeType = map(t)
	implicit def toJava(t: PokeType): Type = t.toJava
 
	
	// VALUES   ---------------------------
	
	case object Normal extends PokeType
	{
		override def toJava: Type = Type.NORMAL
	}
	case object Fighting extends PokeType
	{
		override def toJava: Type = Type.FIGHTING
	}
	case object Flying extends PokeType
	{
		override def toJava: Type = Type.FLYING
	}
	case object Grass extends PokeType
	{
		override def toJava: Type = Type.GRASS
	}
	case object Water extends PokeType
	{
		override def toJava: Type = Type.WATER
	}
	case object Fire extends PokeType
	{
		override def toJava: Type = Type.FIRE
	}
	case object Rock extends PokeType
	{
		override def toJava: Type = Type.ROCK
	}
	case object Ground extends PokeType
	{
		override def toJava: Type = Type.GROUND
	}
	case object Psychic extends PokeType
	{
		override def toJava: Type = Type.PSYCHIC
	}
	case object Bug extends PokeType
	{
		override def toJava: Type = Type.BUG
	}
	case object Dragon extends PokeType
	{
		override def toJava: Type = Type.DRAGON
	}
	case object Electric extends PokeType
	{
		override def toJava: Type = Type.ELECTRIC
	}
	case object Ghost extends PokeType
	{
		override def toJava: Type = Type.GHOST
	}
	case object Poison extends PokeType
	{
		override def toJava: Type = Type.POISON
	}
	case object Ice extends PokeType
	{
		override def toJava: Type = Type.ICE
	}
	case object Steel extends PokeType
	{
		override def toJava: Type = Type.STEEL
	}
	case object Dark extends PokeType
	{
		override def toJava: Type = Type.DARK
	}
	case object Fairy extends PokeType
	{
		override def toJava: Type = Type.FAIRY
	}
}
