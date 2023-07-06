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
	
	protected def strength: Int
	
	
	// IMPLEMENTED  ---------------------
	
	override def self = this
	
	override def compareTo(o: TypeRelation) = strength - o.strength
}

object TypeRelation
{
	val values = Vector[TypeRelation](Unrelated, WeakRelative, Relative, StrongRelative)
	
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
