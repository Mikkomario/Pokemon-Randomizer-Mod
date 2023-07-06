package vf.model

import com.dabomstew.pkrandom.pokemon.Type
import utopia.flow.collection.CollectionExtensions._
import utopia.flow.collection.immutable.Graph
import utopia.flow.util.NotEmpty
import vf.model.TypeRelation.{Relative, StrongRelative, WeakRelative}
import vf.util.RandomUtils._

object TypeRelations
{
	// ATTRIBUTES   -------------------------
	
	private val relations = Graph[Type, TypeRelation](Set(
		(Type.NORMAL, StrongRelative, Type.PSYCHIC),
		(Type.NORMAL, StrongRelative, Type.FIGHTING),
		(Type.NORMAL, Relative, Type.FAIRY),
		(Type.NORMAL, Relative, Type.DARK),
		(Type.PSYCHIC, StrongRelative, Type.FAIRY),
		(Type.PSYCHIC, StrongRelative, Type.DARK),
		(Type.PSYCHIC, Relative, Type.GHOST),
		(Type.PSYCHIC, Relative, Type.ELECTRIC),
		(Type.FAIRY, Relative, Type.DARK),
		(Type.FAIRY, Relative, Type.DRAGON),
		(Type.FAIRY, Relative, Type.ELECTRIC),
		(Type.FAIRY, Relative, Type.GRASS),
		(Type.FAIRY, WeakRelative, Type.GHOST),
		(Type.FAIRY, WeakRelative, Type.FLYING),
		(Type.FAIRY, WeakRelative, Type.WATER),
		(Type.FAIRY, WeakRelative, Type.ICE),
		(Type.FAIRY, WeakRelative, Type.POISON),
		(Type.DARK, StrongRelative, Type.GHOST),
		(Type.DARK, StrongRelative, Type.FIGHTING),
		(Type.DARK, Relative, Type.POISON),
		(Type.FIGHTING, Relative, Type.GROUND),
		(Type.FIGHTING, Relative, Type.POISON),
		(Type.GHOST, Relative, Type.POISON),
		(Type.GHOST, WeakRelative, Type.FLYING),
		(Type.GHOST, WeakRelative, Type.ICE),
		(Type.ELECTRIC, Relative, Type.FIRE),
		(Type.ELECTRIC, Relative, Type.STEEL),
		(Type.ELECTRIC, WeakRelative, Type.WATER),
		(Type.ELECTRIC, WeakRelative, Type.ICE),
		(Type.ELECTRIC, WeakRelative, Type.FLYING),
		(Type.ELECTRIC, WeakRelative, Type.BUG),
		(Type.FIRE, StrongRelative, Type.DRAGON),
		(Type.FIRE, Relative, Type.ROCK),
		(Type.FIRE, WeakRelative, Type.GROUND),
		(Type.DRAGON, WeakRelative, Type.FLYING),
		(Type.DRAGON, WeakRelative, Type.WATER),
		(Type.DRAGON, WeakRelative, Type.ICE),
		(Type.WATER, StrongRelative, Type.ICE),
		(Type.WATER, Relative, Type.FLYING),
		(Type.ICE, WeakRelative, Type.FLYING),
		(Type.POISON, StrongRelative, Type.GRASS),
		(Type.POISON, Relative, Type.BUG),
		(Type.POISON, Relative, Type.GROUND),
		(Type.GRASS, Relative, Type.GROUND),
		(Type.GRASS, WeakRelative, Type.BUG),
		(Type.GROUND, StrongRelative, Type.ROCK),
		(Type.GROUND, WeakRelative, Type.BUG),
		(Type.BUG, WeakRelative, Type.STEEL),
		(Type.ROCK, StrongRelative, Type.STEEL)
	), isTwoWayBound = true)
	
	
	// OTHER    -----------------------
	
	def of(t: Type) =
		apply(t, relations.node(t).leavingEdges.map { e => e.value -> e.end.value }.toVector.asMultiMap)
}

/**
 * Contains all type relations concerning a single type
 * @author Mikko Hilpinen
 * @since 6.7.2023, v1.0-alt
 */
case class TypeRelations(origin: Type, relatives: Map[TypeRelation, Iterable[Type]])
{
	def random(chances: Map[TypeRelation, Double]) = {
		// Case: No type relations => Yields the original type
		if (relatives.isEmpty)
			origin
		// Case: Type relations => Selects a random relation level and then a random type from that level
		else {
			val categoryOptions = NotEmpty(
				chances.flatMap { case (relation, weight) =>
					relatives.get(relation).map { types => relation -> (weight * types.size) }
				})
				.getOrElse { relatives.view.mapValues { _.size.toDouble } }
			val category = weighedRandom(categoryOptions)
			randomFrom(relatives(category).toSeq)
		}
	}
	
	def -(t: Type) =
		copy(relatives = relatives.view.mapValues { _.filterNot { _ == t } }.filterNot { _._2.isEmpty }.toMap)
	def --(types: Iterable[Type]) =
		copy(relatives = relatives.view
			.mapValues { _.filterNot { t => types.exists { _ == t } } }.filterNot { _._2.isEmpty }.toMap)
}
