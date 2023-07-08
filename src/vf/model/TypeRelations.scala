package vf.model

import com.dabomstew.pkrandom.pokemon.Type
import com.dabomstew.pkrandom.romhandlers.RomHandler
import utopia.flow.collection.CollectionExtensions._
import utopia.flow.collection.immutable.{Graph, Pair}
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
	
	// TODO: Consider adding caching (RomHander makes it difficult, though)
	def of(types: TypeSet)(implicit rom: RomHandler) = {
		val primaryRelations = relationsOf(types.primary)
		types.secondary match {
			// Case: Has two types => Finds the combined relations
			case Some(secondary) =>
				val secondaryRelations = relationsOf(secondary)
				val bothRelations = Pair(primaryRelations, secondaryRelations)
				// Merges the two sets of relations
				val mergedRelations = TypeRelation.values.flatMap { level =>
					lazy val strongerLevels = level.stronger
					lazy val nextLevel = level.nextStronger
					lazy val muchStrongerLevels = nextLevel.stronger
					val (separate, common) = bothRelations.map { _(level) }.separateMatching
					
					// Relations appearing in only one type are added,
					// unless they appear in a stronger relation in the other type
					separate.merge { _ ++ _ }
						.filterNot { t => t == types.primary || t == secondary }
						.filterNot { t => strongerLevels.exists { strongerLevel =>
							bothRelations.exists { _(strongerLevel).contains(t) }
						}}
						.map { level -> _ } ++
						// Relations that appear in both types are made stronger
						common.map { _.first }
							.filterNot { t =>
								muchStrongerLevels.exists { strongerLevel =>
									bothRelations.exists { _(strongerLevel).contains(t) }
								}
							}.map { nextLevel -> _ }
				}.asMultiMap
				apply(types, mergedRelations)
			// Case: Has a single type => Finds the relations of that type
			case None => apply(types, primaryRelations)
		}
	}
	
	def of(t: Type)(implicit rom: RomHandler) =
		apply(TypeSet(t), relationsOf(t))
	
	private def relationsOf(t: Type)(implicit rom: RomHandler) =
		relations.node(t).leavingEdges.iterator
			// Removes all types that don't appear in the targeted game
			.filter { e => rom.typeInGame(e.end.value) }
			.map { e => e.value -> e.end.value }
			.toVector.asMultiMap
			.withDefaultValue(Vector())
}

/**
 * Contains all type relations concerning a single type
 * @author Mikko Hilpinen
 * @since 6.7.2023, v1.0-alt
 */
case class TypeRelations(origin: TypeSet, relatives: Map[TypeRelation, Iterable[Type]])
{
	// COMPUTED ----------------------
	
	def random(weights: Map[TypeRelation, Double]) = {
		// Case: No type relations => Yields the original type
		if (relatives.isEmpty)
			origin.random
		// Case: Type relations => Selects a random relation level and then a random type from that level
		else {
			val categoryOptions = NotEmpty(
				weights.flatMap { case (relation, weight) =>
					relatives.get(relation).map { types => relation -> (weight * types.size) }
				})
				.getOrElse { relatives.view.mapValues { _.size.toDouble } }
			val category = weighedRandom(categoryOptions)
			randomFrom(relatives(category).toSeq)
		}
	}
	
	
	// OTHER    ---------------------
	
	def -(t: Type) =
		copy(relatives = relatives.view.mapValues { _.filterNot { _ == t } }.filterNot { _._2.isEmpty }.toMap)
	def --(types: Iterable[Type]) =
		copy(relatives = relatives.view
			.mapValues { _.filterNot { t => types.exists { _ == t } } }.filterNot { _._2.isEmpty }.toMap)
}
