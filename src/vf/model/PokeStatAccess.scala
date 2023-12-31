package vf.model

import com.dabomstew.pkrandom.RandomSource
import com.dabomstew.pkrandom.pokemon.Pokemon
import vf.model.PokeStatAccess.maxValue

/**
 * Enumeration for pokemon stats, such as attack and defence
 * @author Mikko Hilpinen
 * @since 7.7.2023, v1.0-alt
 */
@deprecated("Please use Stat and PokeExtensions instead", "v1.0")
sealed trait PokeStatAccess
{
	// ABSTRACT --------------------
	
	def minimumValue: Int
	def of(poke: Pokemon): Int
	
	/**
	 * @param poke Pokemon to which to apply this stat
	 * @param value New stat value to apply. Within min & max range.
	 */
	protected def _to(poke: Pokemon, value: Int): Unit
	
	
	// COMPUTED -------------------
	
	def otherRandom = {
		val options = PokeStatAccess.values.filterNot { _ == this }
		options(RandomSource.nextInt(options.size))
	}
	
	
	// OTHER    ------------------
	
	def to(poke: Pokemon, value: Int) = _to(poke, (value min maxValue) max minimumValue)
}

@deprecated("Please use Stat and PokeExtensions instead", "v1.0")
object PokeStatAccess
{
	// ATTRIBUTES   ---------------
	
	val maxValue = 255
	
	val values = Vector[PokeStatAccess](Hp, Attack, SpecialAttack, Defense, SpecialDefense, Speed)
	
	
	// COMPUTED -------------------
	
	def random = values(RandomSource.nextInt(values.size))
	
	
	// VALUES   -------------------
	
	case object Hp extends PokeStatAccess
	{
		override def minimumValue: Int = 20
		override def of(poke: Pokemon): Int = poke.hp
		override protected def _to(poke: Pokemon, value: Int): Unit = poke.hp = value
	}
	case object Attack extends PokeStatAccess
	{
		override def minimumValue: Int = 10
		override def of(poke: Pokemon): Int = poke.attack
		override protected def _to(poke: Pokemon, value: Int): Unit = poke.attack = value
	}
	case object SpecialAttack extends PokeStatAccess
	{
		override def minimumValue: Int = 10
		override def of(poke: Pokemon): Int = poke.spatk
		override protected def _to(poke: Pokemon, value: Int): Unit = poke.spatk = value
	}
	case object Defense extends PokeStatAccess
	{
		override def minimumValue: Int = 10
		override def of(poke: Pokemon): Int = poke.defense
		override protected def _to(poke: Pokemon, value: Int): Unit = poke.defense = value
	}
	case object SpecialDefense extends PokeStatAccess
	{
		override def minimumValue: Int = 10
		override def of(poke: Pokemon): Int = poke.spdef
		override protected def _to(poke: Pokemon, value: Int): Unit = poke.spdef = value
	}
	case object Speed extends PokeStatAccess
	{
		override def minimumValue: Int = 10
		override def of(poke: Pokemon): Int = poke.speed
		override protected def _to(poke: Pokemon, value: Int): Unit = poke.speed = value
	}
}