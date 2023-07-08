package vf.controller

import com.dabomstew.pkrandom.RandomSource
import com.dabomstew.pkrandom.pokemon.Type
import com.dabomstew.pkrandom.romhandlers.RomHandler
import utopia.flow.collection.CollectionExtensions._
import vf.model.TypeRelation.{Relative, StrongRelative, Unrelated, WeakRelative}
import vf.model.{EvolveGroup, TypeRelation, TypeRelations, Types}
import vf.util.PokemonExtensions._

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
	
	def all()(implicit groups: IterableOnce[EvolveGroup], types: Types, rom: RomHandler): (Map[Int, (Type, Type)], Map[Int, Type]) = {
		val (conversions, additions) = groups.iterator.splitFlatMap(apply)
		conversions.toMap -> additions.toMap
	}
	
	// Returns
	// 1) All type swaps (bound to pokemon number)
	// 2) All type additions (bound to pokemon number)
	def apply(group: EvolveGroup)(implicit types: Types, rom: RomHandler): (Map[Int, (Type, Type)], Map[Int, Type]) = {
		// Case: Adds a secondary type
		if (group.canAddSecondaryType && RandomSource.nextDouble() < addSecondaryChance) {
			val baseType = types(group.finalForm).primary
			val newType = TypeRelations.of(baseType).random(addSecondaryChances)
			
			// May also alter the primary type(s) (lowered chance)
			val primaryTypeChanges: Map[Int, (Type, Type)] = {
				if (RandomSource.nextDouble() < changePrimaryChance * 0.5) {
					val conversions = group.primaryTypes.map { original =>
						// Won't allow conversion to the new secondary type
						val target = (TypeRelations.of(original) - newType).random(changePrimaryChances)
						original -> target
					}.toMap
					// Applies type conversions
					group.iterator.flatMap { poke =>
						val originalTypes = poke.types
						val primaryConversion = conversions.get(originalTypes.primary).map { newType =>
							poke.primaryType = newType
							poke.number -> (originalTypes.primary -> newType)
						}
						val secondaryConversion = originalTypes.secondary.flatMap { secondary =>
							conversions.get(secondary).map { newType =>
								poke.secondaryType = newType
								poke.number -> (secondary -> newType)
							}
						}
						primaryConversion ++ secondaryConversion
					}.toMap
				}
				else
					Map()
			}
			
			// Adds secondary type from top to bottom
			// Doesn't necessarily add the type to all forms
			val addedTypes = (group.finalForm +:
				group.finalToBaseIterator.drop(1).takeWhile { _ => RandomSource.nextDouble() < addSecondaryChainingChance })
				.map { poke =>
					poke.secondaryType = newType
					poke.number -> newType
				}
				.toMap
			// May randomly swap the secondary type of the mega form
			val (megaTypeSwaps, megaTypeAdditions) = group.megas.flatDivideWith { mega =>
				val original = mega.types
				original.secondary match {
					// Case: Mega form has two types => May swap the other type
					case Some(original) =>
						if (RandomSource.nextDouble() < changeSecondaryChance) {
							val newType = TypeRelations.of(original).random(changeSecondaryChances)
							mega.secondaryType = newType
							Some(Left(mega.number -> (original -> newType)))
						}
						else
							None
					// Case: Mega form has one type => Adds the new secondary type
					case None =>
						mega.secondaryType = newType
						Some(Right(mega.number -> newType))
				}
			}
			
			(primaryTypeChanges ++ megaTypeSwaps) -> (addedTypes ++ megaTypeAdditions)
		}
		// Case: Swaps the secondary type(s)
		else if (group.hasSecondaryTypes && RandomSource.nextDouble() < changeSecondaryChance) {
			val primaryTypes = group.primaryTypes
			val conversions = group.secondaryTypes
				.map { t => t -> (TypeRelations.of(t) -- primaryTypes).random(changeSecondaryChances) }
				.toMap
			val secondaryTypeSwaps = group.iterator.flatMap { poke =>
				val original = poke.types
				original.secondary.flatMap { secondary =>
					conversions.get(secondary).map { newType =>
						poke.secondaryType = newType
						poke.number -> (secondary -> newType)
					}
				}
			}.toMap
			
			// May also change the primary type (lowered chance)
			val allTypeSwaps = {
				if (RandomSource.nextDouble() < changePrimaryChance * 0.5) {
					val newSecondaryTypes = conversions.valuesIterator.toSet
					val primaryConversions = primaryTypes
						.map { t => t -> (TypeRelations.of(t) -- newSecondaryTypes).random(changePrimaryChances) }
						.toMap
					applyPrimaryConversions(group, primaryConversions) ++ secondaryTypeSwaps
				}
				else
					secondaryTypeSwaps
			}
			allTypeSwaps -> Map()
		}
		// Case: Swaps the primary type(s)
		else if (RandomSource.nextDouble() < changePrimaryChance) {
			val secondaryTypes = group.secondaryTypes
			val conversions = group.primaryTypes
				.map { t => t -> (TypeRelations.of(t) -- secondaryTypes).random(changePrimaryChances) }
				.toMap
			applyPrimaryConversions(group, conversions) -> Map()
		}
		else
			Map[Int, (Type, Type)]() -> Map[Int, Type]()
	}
	
	private def applyPrimaryConversions(group: EvolveGroup, conversions: Map[Type, Type])(implicit types: Types) =
		group.iterator.flatMap { poke =>
			val original = poke.types
			conversions.get(original.primary).map { newType =>
				poke.primaryType = newType
				poke.number -> (original.primary -> newType)
			}
		}.toMap
}
