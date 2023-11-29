package vf.controller

import com.dabomstew.pkrandom.RandomSource
import com.dabomstew.pkrandom.pokemon.Type
import com.dabomstew.pkrandom.romhandlers.RomHandler
import utopia.flow.collection.CollectionExtensions._
import vf.model.TypeRelation.{Relative, StrongRelative, Unrelated, WeakRelative}
import vf.model.{EvolveGroup, TypeRelation, TypeRelations}

import java.io.PrintWriter
import scala.jdk.CollectionConverters._

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
	
	private val changePrimaryWeights = Map[TypeRelation, Double](
		StrongRelative -> 0.55, Relative -> 0.3, WeakRelative -> 0.15)
	private val changeSecondaryWeights = Map[TypeRelation, Double](
		StrongRelative -> 0.5, Relative -> 0.25, WeakRelative -> 0.15, Unrelated -> 0.1)
	private val addSecondaryWeights = Map[TypeRelation, Double](
		StrongRelative -> 0.35, Relative -> 0.3, WeakRelative -> 0.25, Unrelated -> 0.1)
	
	
	// OTHER    --------------------------
	
	def all(groups: Iterable[EvolveGroup])(implicit rom: RomHandler): (Map[Int, Map[Type, Type]], Map[Int, Type]) = {
		Log("types") { writer =>
			val (conversions, additions) = groups.splitFlatMap { apply(_, writer) }
			// Logs the changed types
			writer.println("\n\n------------------")
			groups.foreach { group =>
				group.iterator.foreach { poke =>
					if (poke.originalState.types != poke.types)
						writer.println(s"${poke.name}: ${ poke.originalState.types } => ${ poke.types }")
				}
			}
			conversions.toMap -> additions.toMap
		}
	}
	
	// Assigns correct types to cosmetic forms
	def fixCosmeticForms()(implicit rom: RomHandler) = rom.getPokemonInclFormes.iterator().asScala
		.flatMap(Option.apply).filter { _.actuallyCosmetic }
		.foreach { form =>
			form.primaryType = form.baseForme.primaryType
			form.secondaryType = form.baseForme.secondaryType
		}
	
	// Returns
	// 1) All type swaps (bound to pokemon number)
	// 2) All type additions (bound to pokemon number)
	def apply(group: EvolveGroup, writer: PrintWriter)(implicit rom: RomHandler): (Map[Int, Map[Type, Type]], Map[Int, Type]) =
	{
		writer.println(s"\nProcessing $group\t-----------------")
		writer.println(s"Original types: ${group.types.mkString(", ")}")
		
		// Case: Adds a secondary type
		if (group.canAddSecondaryType && RandomSource.nextDouble() < addSecondaryChance) {
			val baseType = group.finalForm.primaryType
			val newType = TypeRelations.of(baseType).random(addSecondaryWeights)
			writer.println(s"Adds secondary type: $newType")
			
			// May also alter the primary type(s) (lowered chance)
			val primaryTypeChanges: Map[Int, Map[Type, Type]] = {
				if (RandomSource.nextDouble() < changePrimaryChance * 0.5) {
					val conversions = group.primaryTypes.map { original =>
						// Won't allow conversion to the new secondary type
						val target = (TypeRelations.of(original) - newType).random(changePrimaryWeights)
						original -> target
					}.toMap
					writer.println(s"Also applies ${conversions.size} primary type conversions:")
					conversions.foreach { case (from, to) => writer.println(s"\t- $from => $to") }
					// Applies type conversions
					group.iterator.flatMap { poke =>
						val originalTypes = poke.types
						val primaryConversion = conversions.get(originalTypes.primary).map { newType =>
							poke.primaryType = newType
							Map(originalTypes.primary -> newType)
						}
						val secondaryConversion = originalTypes.secondary.flatMap { secondary =>
							conversions.get(secondary).map { newType =>
								poke.secondaryType = newType
								Map(secondary -> newType)
							}
						}
						// Merges the 0-2 conversions to a single map (or None if both are empty)
						if (primaryConversion.nonEmpty || secondaryConversion.nonEmpty) {
							val bothConversions = primaryConversion.getOrElse(Map[Type, Type]()) ++
								secondaryConversion.getOrElse(Map[Type, Type]())
							Some(poke.number -> bothConversions)
						}
						else
							None
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
							val newType = TypeRelations.of(original).random(changeSecondaryWeights)
							writer.println(s"Swaps the secondary mega type to $newType")
							mega.secondaryType = newType
							Some(Left(mega.number -> Map(original -> newType)))
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
				.map { t => t -> (TypeRelations.of(t) -- primaryTypes).random(changeSecondaryWeights) }
				.toMap
			val secondaryTypeSwaps = group.iterator.flatMap { poke =>
				val original = poke.types
				original.secondary.flatMap { secondary =>
					conversions.get(secondary).map { newType =>
						poke.secondaryType = newType
						writer.println(s"Waps secondary type of ${poke.name} from $secondary to $newType")
						poke.number -> Map(secondary -> newType)
					}
				}
			}.toMap
			
			// May also change the primary type (lowered chance)
			val allTypeSwaps = {
				if (RandomSource.nextDouble() < changePrimaryChance * 0.5) {
					val newSecondaryTypes = conversions.valuesIterator.toSet
					val primaryConversions = primaryTypes
						.map { t => t -> (TypeRelations.of(t) -- newSecondaryTypes).random(changePrimaryWeights) }
						.toMap
					writer.println(s"Also changes ${primaryConversions.size} primary types:")
					primaryConversions.foreach { case (from, to) => writer.println(s"\t- $from => $to") }
					applyPrimaryConversions(group, primaryConversions).mergeWith(secondaryTypeSwaps) { _ ++ _ }
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
				.map { t => t -> (TypeRelations.of(t) -- secondaryTypes).random(changePrimaryWeights) }
				.toMap
			writer.println(s"Swaps ${conversions.size} primary types:")
			conversions.foreach { case (from, to) => writer.println(s"\t- $from => $to") }
			applyPrimaryConversions(group, conversions) -> Map()
		}
		// Case: No change to types
		else
			Map[Int, Map[Type, Type]]() -> Map[Int, Type]()
	}
	
	private def applyPrimaryConversions(group: EvolveGroup, conversions: Map[Type, Type]) =
		group.iterator.flatMap { poke =>
			val original = poke.types
			conversions.get(original.primary).map { newType =>
				poke.primaryType = newType
				poke.number -> Map(original.primary -> newType)
			}
		}.toMap
}
