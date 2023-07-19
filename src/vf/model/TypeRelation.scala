package vf.model

import utopia.flow.operator.Extreme.{Max, Min}
import utopia.flow.operator.{Extreme, SelfComparable, Sign, Steppable}

/**
 * Represents a relationship between two pokemon types
 * @author Mikko Hilpinen
 * @since 3.7.2023, v1.0-alt
 */
sealed trait TypeRelation extends SelfComparable[TypeRelation] with Steppable[TypeRelation]
{
	// ABSTRACT -------------------------
	
	// Also matches index in list
	protected def strength: Int
	
	
	// IMPLEMENTED  ---------------------
	
	override def self = this
	
	override def next(direction: Sign): TypeRelation = TypeRelation(strength + direction.modifier)
	override def is(extreme: Extreme): Boolean = this == TypeRelation(extreme)
	
	override def compareTo(o: TypeRelation) = strength - o.strength
	
	
	// OTHER    -------------------------
	
	// Selects the average between the two relations. Rounds to the stronger value.
	def avg(other: TypeRelation) = TypeRelation(((strength + other.strength) / 2.0).ceil.toInt)
}

object TypeRelation
{
	// ATTRIBUTES   -------------------
	
	val values = Vector[TypeRelation](Unrelated, WeakRelative, Relative, StrongRelative)
	val min: TypeRelation = Unrelated
	val max: TypeRelation = StrongRelative
	
	
	// OTHER    ----------------------
	
	def apply(extreme: Extreme) = extreme match {
		case Min => min
		case Max => max
	}
	
	private def apply(strength: Int) = values.lift(strength)
		.getOrElse {
			if (strength < min.strength)
				min
			else
				max
		}
	
	
	// VALUES   ----------------------
	
	case object StrongRelative extends TypeRelation
	{
		override protected val strength: Int = 3
	}
	case object Relative extends TypeRelation
	{
		override protected val strength: Int = 2
	}
	case object WeakRelative extends TypeRelation
	{
		override protected val strength: Int = 1
	}
	case object Unrelated extends TypeRelation
	{
		override protected val strength: Int = 0
	}
}
