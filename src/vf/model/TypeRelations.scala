package vf.model

import com.dabomstew.pkrandom.romhandlers.RomHandler
import utopia.flow.collection.CollectionExtensions._
import utopia.flow.collection.immutable.caching.cache.Cache
import utopia.flow.collection.immutable.{Graph, Pair}
import utopia.flow.util.NotEmpty
import vf.poke.core.model.enumeration.PokeType._
import vf.model.TypeRelation.{Relative, StrongRelative, Unrelated, WeakRelative}
import vf.poke.core.model.enumeration.PokeType
import vf.util.RandomUtils._
import vf.util.PokeExtensions._

object TypeRelations
{
	// ATTRIBUTES   -------------------------
	
	private val relations = Graph[PokeType, TypeRelation](Set(
		(Normal, StrongRelative, Psychic),
		(Normal, StrongRelative, Fighting),
		(Normal, Relative, Fairy),
		(Normal, Relative, Dark),
		(Psychic, StrongRelative, Fairy),
		(Psychic, StrongRelative, Dark),
		(Psychic, Relative, Ghost),
		(Psychic, Relative, Electric),
		(Fairy, Relative, Dark),
		(Fairy, Relative, Dragon),
		(Fairy, Relative, Electric),
		(Fairy, Relative, Grass),
		(Fairy, WeakRelative, Ghost),
		(Fairy, WeakRelative, Flying),
		(Fairy, WeakRelative, Water),
		(Fairy, WeakRelative, Ice),
		(Fairy, WeakRelative, Poison),
		(Dark, StrongRelative, Ghost),
		(Dark, StrongRelative, Fighting),
		(Dark, Relative, Poison),
		(Fighting, Relative, Ground),
		(Fighting, Relative, Poison),
		(Ghost, Relative, Poison),
		(Ghost, WeakRelative, Flying),
		(Ghost, WeakRelative, Ice),
		(Electric, Relative, Fire),
		(Electric, Relative, Steel),
		(Electric, WeakRelative, Water),
		(Electric, WeakRelative, Ice),
		(Electric, WeakRelative, Flying),
		(Electric, WeakRelative, Bug),
		(Fire, StrongRelative, Dragon),
		(Fire, Relative, Rock),
		(Fire, WeakRelative, Ground),
		(Dragon, WeakRelative, Flying),
		(Dragon, WeakRelative, Water),
		(Dragon, WeakRelative, Ice),
		(Water, StrongRelative, Ice),
		(Water, WeakRelative, Flying),
		(Ice, WeakRelative, Flying),
		(Poison, StrongRelative, Grass),
		(Poison, Relative, Bug),
		(Poison, Relative, Ground),
		(Grass, Relative, Ground),
		(Grass, WeakRelative, Bug),
		(Ground, StrongRelative, Rock),
		(Ground, WeakRelative, Bug),
		(Bug, WeakRelative, Steel),
		(Rock, StrongRelative, Steel)
	), isTwoWayBound = true)
	
	// Caches calculated relations for optimization
	// Rom => Type => Relatives
	private val typeRelativesCache = Cache { implicit rom: RomHandler => Cache(relationsOf) }
	// Rom => TypeSet => TypeRelations
	private val typeSetRelativesCache = Cache { implicit rom: RomHandler => Cache(_of) }
	
	
	// OTHER    -----------------------
	
	def of(types: TypeSet)(implicit rom: RomHandler) = typeSetRelativesCache(rom)(types)
	def of(t: PokeType)(implicit rom: RomHandler) = apply(TypeSet(t), typeRelativesCache(rom)(t))
	
	private def _of(types: TypeSet)(implicit rom: RomHandler) = {
		val typeRelatives = typeRelativesCache(rom)
		val primaryRelations = typeRelatives(types.primary)
		types.secondary match {
			// Case: Has two types => Finds the combined relations
			case Some(secondary) =>
				val secondaryRelations = typeRelatives(secondary)
				val bothRelations = Pair(primaryRelations, secondaryRelations)
				// Merges the two sets of relations
				val mergedRelations = TypeRelation.values.flatMap { level =>
					lazy val strongerLevels = level.moreIterator.toVector
					lazy val nextLevel = level.more
					lazy val muchStrongerLevels = nextLevel.moreIterator.toVector
					val (separate, common) = bothRelations.map { _(level) }.separateMatching
					
					// Relations that appear in both types are made stronger (unless unrelated)
					val mergedCommon = {
						if (level == Unrelated)
							Vector()
						else
							common.map { _.first }
								.filterNot { t =>
									muchStrongerLevels.exists { strongerLevel =>
										bothRelations.exists { _(strongerLevel).contains(t) }
									}
								}.map { nextLevel -> _ }
					}
					// Relations appearing in only one type are added,
					// unless they appear in a stronger relation in the other type
					separate.merge { _ ++ _ }
						.filterNot { t => t == types.primary || t == secondary }
						.filterNot { t =>
							// Also removes the origin types themselves from these relations
							types.contains(t) || strongerLevels.exists { strongerLevel =>
								bothRelations.exists { _(strongerLevel).contains(t) }
							}
						}
						.map { level -> _ } ++ mergedCommon
				}.asMultiMap
				apply(types, mergedRelations)
			// Case: Has a single type => Finds the relations of that type
			case None => apply(types, primaryRelations)
		}
	}
	
	private def relationsOf(t: PokeType)(implicit rom: RomHandler) = {
		val related = relations.node(t).leavingEdges.iterator
			// Removes all types that don't appear in the targeted game
			.filter { e => rom.typeInGame(e.end.value) }
			.map { e => e.value -> e.end.value }
			.toVector
		val unrelated = PokeType.values.filter { t => rom.typeInGame(t) && !related.exists { _._2 == t } }
		val relatedMap = relations.node(t).leavingEdges.iterator
			// Removes all types that don't appear in the targeted game
			// Also removes the type itself
			.filter { e =>
				val relatedType = e.end.value
				relatedType != t && rom.typeInGame(relatedType)
			}
			.map { e => e.value -> e.end.value }
			.toVector.asMultiMap
		(relatedMap + (Unrelated -> unrelated)).withDefaultValue(Vector())
	}
}

/**
 * Contains all type relations concerning a single type
 * @author Mikko Hilpinen
 * @since 6.7.2023, v1.0-alt
 */
case class TypeRelations(origin: TypeSet, relatives: Map[TypeRelation, Iterable[PokeType]])
{
	// ATTRIBUTES   ------------------
	
	lazy val relationStrengths = (relatives.flatMap { case (strength, types) => types.map { t => t -> strength } } ++
		origin.types.map { _ -> StrongRelative }).withDefaultValue(Unrelated)
	
	
	// COMPUTED ----------------------
	
	def random(weights: Map[TypeRelation, Double], additionalWeights: Map[PokeType, Double] = Map()) = {
		// Case: No type relations => Yields the original type
		if (relatives.isEmpty)
			origin.random
		else if (weights.isEmpty) {
			// Case: Other weights specified => Selects weighed random from relatives
			if (additionalWeights.nonEmpty) {
				val options = WeakRelative.andLarger
					.flatMap { relation =>
						relatives.getOrElse(relation, Vector.empty).map { t => t -> additionalWeights.getOrElse(t, 1.0) }
					}
					.toVector
				if (options.exists { _._2 > 0 })
					weighedRandom(options)
				else
					origin.random
			}
			// Case: No weights specified => Returns a random relative type
			else
				NotEmpty(WeakRelative.andLarger.flatMap { relatives.getOrElse(_, Vector.empty) }.toVector)
					.getOrElse(origin.types).random
		}
		// Case: Type relations => Applies relation-level based weights to types and selects one randomly
		else {
			val weighedTypes = weights
				.flatMap { case (relation, relationWeight) =>
					relatives.getOrElse(relation, Vector.empty)
						.map { t => t -> (relationWeight * additionalWeights.getOrElse(t, 1.0)) }
				}
				.toVector
			if (weighedTypes.exists { _._2 > 0 })
				weighedRandom(weighedTypes)
			else
				origin.random
		}
	}
	
	
	// IMPLEMENTED  -----------------
	
	override def toString = s"Relations of $origin are ${
		relatives.toVector.reverseSortBy { _._1 }.map { case (relation, types) =>
			s"$relation: [${ types.toVector.map { _.toString }.sorted.mkString(", ") }]"
		}.mkString(", ") }"
	
	
	// OTHER    ---------------------
	
	// Finds the type relation strength with another type-set
	def apply(types: TypeSet) = types.types.iterator.map(relationStrengths.apply).reduce { _ avg _ }
	
	def -(t: PokeType) =
		copy(relatives = relatives.view.mapValues { _.filterNot { _ == t } }.filterNot { _._2.isEmpty }.toMap)
	def --(types: Iterable[PokeType]) =
		copy(relatives = relatives.view
			.mapValues { _.filterNot { t => types.exists { _ == t } } }.filterNot { _._2.isEmpty }.toMap)
}
