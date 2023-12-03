package vf.model

import com.dabomstew.pkrandom.pokemon.{Effectiveness, Type}
import utopia.flow.collection.immutable.caching.cache.Cache
import utopia.flow.operator.sign.{Sign, UncertainSign}
import vf.poke.core.model.enumeration.PokeType
import vf.util.PokeExtensions._

import scala.jdk.CollectionConverters._

object EffectivenessRelations
{
	// ATTRIBUTES   -------------------------
	
	// The first keys are defending types
	// The second keys are offensive types
	// The values are effectiveness levels
	private val cache = Cache { against: TypeSet =>
		Effectiveness.against(against.primary.toJava, against.secondary.map { _.toJava }.orNull, 6).asScala.toMap
	}
	private val relationsCache = Cache { t: TypeSet => new EffectivenessRelations(t) }
	
	lazy val averageSingleTypeDefenseRating =
		PokeType.values.iterator.map { apply(_).defenseRating }.sum / Type.values().length
	lazy val averageSingleTypeOffenseRating =
		PokeType.values.iterator.map { apply(_).offenseRating }.sum / Type.values().length
	
	
	// OTHER    --------------------------
	
	def apply(t: PokeType): EffectivenessRelations = apply(TypeSet(t))
	def apply(t: TypeSet): EffectivenessRelations = relationsCache(t)
	
	private def offenseValueOf(effectiveness: Effectiveness) = effectiveness match {
		case Effectiveness.NEUTRAL => 0.0
		case Effectiveness.HALF => -1.0
		case Effectiveness.QUARTER => -1.5
		case Effectiveness.ZERO => -3
		case Effectiveness.DOUBLE => 1.0
		case Effectiveness.QUADRUPLE => 3.0
	}
	private def defenseValueOf(effectiveness: Effectiveness) = -offenseValueOf(effectiveness)
}

/**
 * Represents the type effectiveness -relationships of some type
 * @author Mikko Hilpinen
 * @since 28.11.2023, v1.0-alt
 */
case class EffectivenessRelations(types: TypeSet)
{
	import EffectivenessRelations._
	
	// ATTRIBUTES   ------------------------
	
	/**
	 * Overall defensive power of this type combo, where 0 is neutral, negative is weak and positive is strong
	 */
	lazy val defenseRating = cache(types).valuesIterator.map(defenseValueOf).sum
	/**
	 * Overall defensive power of this type combo, where 0 is neutral, negative is weak and positive is strong
	 */
	lazy val offenseRating = PokeType.values.iterator.map { defendingType =>
		val map = cache(TypeSet(defendingType))
		types.types.map { attackingType => offenseValueOf(map(attackingType)) }.max
	}.sum
	
	lazy val relativeDefenseRating = defenseRating / averageSingleTypeDefenseRating
	lazy val relativeOffenseRating = offenseRating / averageSingleTypeOffenseRating
	
	lazy val defensiveWeaknesses = PokeType.values.iterator.filter { defenseRatingAgainst(_) < 0 }.toSet
	lazy val offensiveWeaknesses = PokeType.values.iterator.filter { offenseRatingAgainst(_) < 0 }.toSet
	
	
	// OTHER    --------------------------
	
	def defenseRatingAgainst(damageType: PokeType): Double = defenseValueOf(cache(types)(damageType))
	def defenseRatingAgainst(other: TypeSet): Double = other.types.map(defenseRatingAgainst).min
	
	def offenseRatingAgainst(target: PokeType): Double = offenseRatingAgainst(TypeSet(target))
	def offenseRatingAgainst(targetType: TypeSet) =
		types.types.map { t => offenseValueOf(cache(targetType)(t)) }.max
	
	/**
	 * @param other Opposing type
	 * @return Positive if this type is strong against the other type.
	 *         Negative if this type is weak against the other type.
	 *         Uncertain if mixed or neutral.
	 */
	def against(other: TypeSet): UncertainSign = {
		val defenseSign = Sign.of(defenseRatingAgainst(other))
		val offenseSign = Sign.of(offenseRatingAgainst(other))
		defenseSign.binary match {
			case Some(defenseSign) => if (offenseSign == defenseSign.opposite) UncertainSign else defenseSign
			case None => offenseSign
		}
	}
}
