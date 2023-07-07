package vf.model

import com.dabomstew.pkrandom.RandomSource
import com.dabomstew.pkrandom.pokemon.Pokemon

/**
 * Enumeration for pokemon stats, such as attack and defence
 * @author Mikko Hilpinen
 * @since 7.7.2023, v1.0-alt
 */
sealed trait PokeStat
{
	// ABSTRACT --------------------
	
	def minimumValue: Int
	def of(poke: Pokemon): Int
	def to(poke: Pokemon, value: Int): Unit
	
	
	// COMPUTED -------------------
	
	def otherRandom = {
		val options = PokeStat.values.filterNot { _ == this }
		options(RandomSource.nextInt(options.size))
	}
}

object PokeStat
{
	// ATTRIBUTES   ---------------
	
	val maxValue = 255
	
	val values = Vector[PokeStat](Hp, Attack, SpecialAttack, Defense, SpecialDefense, Speed)
	
	
	// COMPUTED -------------------
	
	def random = values(RandomSource.nextInt(values.size))
	
	
	// VALUES   -------------------
	
	case object Hp extends PokeStat
	{
		override def minimumValue: Int = 20
		override def of(poke: Pokemon): Int = poke.hp
		override def to(poke: Pokemon, value: Int): Unit = poke.hp = value
	}
	case object Attack extends PokeStat
	{
		override def minimumValue: Int = 10
		override def of(poke: Pokemon): Int = poke.attack
		override def to(poke: Pokemon, value: Int): Unit = poke.attack = value
	}
	case object SpecialAttack extends PokeStat
	{
		override def minimumValue: Int = 10
		override def of(poke: Pokemon): Int = poke.spatk
		override def to(poke: Pokemon, value: Int): Unit = poke.spatk = value
	}
	case object Defense extends PokeStat
	{
		override def minimumValue: Int = 10
		override def of(poke: Pokemon): Int = poke.defense
		override def to(poke: Pokemon, value: Int): Unit = poke.defense = value
	}
	case object SpecialDefense extends PokeStat
	{
		override def minimumValue: Int = 10
		override def of(poke: Pokemon): Int = poke.spdef
		override def to(poke: Pokemon, value: Int): Unit = poke.spdef = value
	}
	case object Speed extends PokeStat
	{
		override def minimumValue: Int = 10
		override def of(poke: Pokemon): Int = poke.speed
		override def to(poke: Pokemon, value: Int): Unit = poke.speed = value
	}
}