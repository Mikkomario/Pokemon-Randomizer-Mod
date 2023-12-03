package vf.util

import com.dabomstew.pkrandom.pokemon.{Pokemon, Type}
import vf.poke.core.model.enumeration.{PokeType, Stat}
import vf.poke.core.model.enumeration.PokeType._
import Stat._

import scala.language.implicitConversions

/**
 * Extensions for dealing with poke models
 * @author Mikko Hilpinen
 * @since 1.12.2023, v1.0
 */
object PokeExtensions
{
	// ATTRIBUTES   -------------------------
	
	val typePairs = Vector[(PokeType, Type)](Normal -> Type.NORMAL, Fighting -> Type.FIGHTING, Flying -> Type.FLYING,
		Grass -> Type.GRASS, Water -> Type.WATER, Fire -> Type.FIRE, Rock -> Type.ROCK, Ground -> Type.GROUND,
		Psychic -> Type.PSYCHIC, Bug -> Type.BUG, Dragon -> Type.DRAGON,
		Electric -> Type.ELECTRIC, Ghost -> Type.GHOST, Poison -> Type.POISON, Ice -> Type.ICE,
		Steel -> Type.STEEL, Dark -> Type.DARK, Fairy -> Type.FAIRY)
	val scalaToJavaType = typePairs.toMap
	val javaToScalaType = typePairs.map { case (s, j) => j -> s }.toMap
	
	
	// IMPLICIT CONVERSIONS -----------------
	
	implicit def scalaTypeToJavaType(t: PokeType): Type = scalaToJavaType(t)
	implicit def javaTypeToScalaType(t: Type): PokeType = javaToScalaType(t)
	
	
	// EXTENSIONS   -------------------------
	
	implicit class RichPokeType(val t: PokeType) extends AnyVal
	{
		def toJava = scalaToJavaType(t)
	}
	
	implicit class RichType(val t: Type) extends AnyVal
	{
		def toScala = javaToScalaType(t)
	}
	
	implicit class RichPokemon(val p: Pokemon) extends AnyVal
	{
		def apply(stat: Stat) = stat match {
			case Hp => p.hp
			case Attack => p.attack
			case Defense => p.defense
			case SpecialAttack => p.spatk
			case SpecialDefense => p.spdef
			case Speed => p.speed
		}
		def update(stat: Stat, value: Int) = {
			val appliedValue = (value max stat.minimumValue) min Stat.maxValue
			stat match {
				case Hp => p.hp = appliedValue
				case Attack => p.attack = appliedValue
				case Defense => p.defense = appliedValue
				case SpecialAttack => p.spatk = appliedValue
				case SpecialDefense => p.spdef = appliedValue
				case Speed => p.speed = appliedValue
			}
		}
	}
}
