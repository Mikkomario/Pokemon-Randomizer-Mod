package vf.controller

import com.dabomstew.pkrandom.RandomSource
import com.dabomstew.pkrandom.pokemon.Type
import utopia.flow.collection.CollectionExtensions._
import vf.model.TypeRelation.{Relative, StrongRelative, Unrelated, WeakRelative}
import vf.model.{EvolveGroup, TypeRelation, TypeRelations, Types}

/**
 * Randomizes pokemon types
 * @author Mikko Hilpinen
 * @since 3.7.2023, v1.0-alt
 */
object RandomizeTypes
{
	// ATTRIBUTES   -----------------------
	
	private val changePrimaryChance = 0.3
	private val changeSecondaryChance = 0.5
	private val addSecondaryChance = 0.65
	private val addSecondaryChainingChance = 0.75
	
	private val changePrimaryChances = Map[TypeRelation, Double](
		StrongRelative -> 0.55, Relative -> 0.3, WeakRelative -> 0.15)
	private val changeSecondaryChances = Map[TypeRelation, Double](
		StrongRelative -> 0.5, Relative -> 0.25, WeakRelative -> 0.15, Unrelated -> 0.1)
	private val addSecondaryChances = Map[TypeRelation, Double](
		StrongRelative -> 0.35, Relative -> 0.3, WeakRelative -> 0.25, Unrelated -> 0.1)
	
	
	// OTHER    --------------------------
	
	def apply(groups: IterableOnce[EvolveGroup])(implicit types: Types): Unit =
		groups.iterator.foreach(apply)
	
	def apply(group: EvolveGroup)(implicit types: Types) = {
		// Case: Adds a secondary type
		if (group.canAddSecondaryType && RandomSource.nextDouble() < addSecondaryChance) {
			val baseType = types(group.finalForm).primary
			val newType = TypeRelations.of(baseType).random(addSecondaryChances)
			
			// May also alter the primary type(s) (lowered chance)
			if (RandomSource.nextDouble() < changePrimaryChance * 0.5) {
				val conversions = group.primaryTypes.map { original =>
					// Won't allow conversion to the new secondary type
					val target = (TypeRelations.of(original) - newType).random(changePrimaryChances)
					original -> target
				}.toMap
				// Applies type conversions
				group.iterator.foreach { poke =>
					conversions.get(poke.primaryType).foreach { poke.primaryType = _ }
					Option(poke.secondaryType).flatMap(conversions.get).foreach { poke.secondaryType = _ }
				}
			}
			
			// Adds secondary type from top to bottom
			// Doesn't necessarily add the type to all forms
			(group.finalForm +:
				group.finalToBaseIterator.drop(1).takeWhile { _ => RandomSource.nextDouble() < addSecondaryChainingChance })
				.foreach { _.secondaryType = newType }
			// May randomly swap the secondary type of the mega form
			group.mega.foreach { mega =>
				Option(mega.secondaryType) match {
					// Case: Mega form has two types => May swap the other type
					case Some(original) =>
						if (RandomSource.nextDouble() < changeSecondaryChance)
							mega.secondaryType = TypeRelations.of(original).random(changeSecondaryChances)
					// Case: Mega form has one type => Adds the new secondary type
					case None => mega.secondaryType = newType
				}
			}
		}
		// Case: Swaps the secondary type(s)
		else if (group.hasSecondaryTypes && RandomSource.nextDouble() < changeSecondaryChance) {
			val primaryTypes = group.primaryTypes
			val conversions = group.secondaryTypes
				.map { t => t -> (TypeRelations.of(t) -- primaryTypes).random(changeSecondaryChances) }
				.toMap
			group.iterator.foreach { poke =>
				Option(poke.secondaryType).flatMap(conversions.get).foreach { poke.secondaryType = _ }
			}
			
			// May also change the primary type (lowered chance)
			if (RandomSource.nextDouble() < changePrimaryChance * 0.5) {
				val newSecondaryTypes = conversions.valuesIterator.toSet
				val primaryConversions = primaryTypes
					.map { t => t -> (TypeRelations.of(t) -- newSecondaryTypes).random(changePrimaryChances) }
					.toMap
				applyPrimaryConversions(group, primaryConversions)
			}
		}
		// Case: Swaps the primary type(s)
		else if (RandomSource.nextDouble() < changePrimaryChance) {
			val secondaryTypes = group.secondaryTypes
			val conversions = group.primaryTypes
				.map { t => t -> (TypeRelations.of(t) -- secondaryTypes).random(changePrimaryChances) }
				.toMap
			applyPrimaryConversions(group, conversions)
		}
	}
	
	private def applyPrimaryConversions(group: EvolveGroup, conversions: Map[Type, Type]) =
		group.iterator.foreach { poke =>
			conversions.get(poke.primaryType).foreach { poke.primaryType = _ }
		}
}
