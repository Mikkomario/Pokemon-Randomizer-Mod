package vf.model

import utopia.flow.operator.SelfComparable

/**
 * Represents a relationship between two pokemon types
 * @author Mikko Hilpinen
 * @since 3.7.2023, v1.0-alt
 */
sealed trait TypeRelation extends SelfComparable[TypeRelation]
{
	// ABSTRACT -------------------------
	
	// Also matches index in list
	protected def strength: Int
	
	
	// COMPUTED ------------------------
	
	def nextStronger = TypeRelation(strength + 1)
	def nextWeaker = TypeRelation(strength - 1)
	
	def stronger = TypeRelation.values.drop(strength + 1)
	def weaker = TypeRelation.values.take(strength)
	
	
	// IMPLEMENTED  ---------------------
	
	override def self = this
	
	override def compareTo(o: TypeRelation) = strength - o.strength
}

object TypeRelation
{
	// ATTRIBUTES   -------------------
	
	val values = Vector[TypeRelation](Unrelated, WeakRelative, Relative, StrongRelative)
	val min: TypeRelation = WeakRelative
	val max: TypeRelation = StrongRelative
	
	
	// OTHER    ----------------------
	
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
