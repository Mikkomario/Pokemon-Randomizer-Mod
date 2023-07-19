package vf.model

import com.dabomstew.pkrandom.pokemon.Pokemon
import com.dabomstew.pkrandom.romhandlers.RomHandler
import com.dabomstew.pkrandom.{RandomSource, RomFunctions}
import utopia.flow.collection.CollectionExtensions._
import utopia.flow.collection.immutable.Pair
import utopia.flow.collection.immutable.range.NumericSpan
import vf.model.EvolveGroup.assumedFormLevelDuration

import scala.collection.immutable.NumericRange
import scala.jdk.CollectionConverters._

object EvolveGroup
{
	// ATTRIBUTES   ----------------------
	
	private val assumedFormLevelDuration = 15
	
	
	// COMPUTED --------------------------
	
	def all(implicit rom: RomHandler) =
		RomFunctions.getBasicPokemon(rom).iterator().asScala
			// Won't group cosmetic forms
			.filterNot { _.actuallyCosmetic }
			.flatMap(allStartingFrom).toSet
	
	
	// OTHER    --------------------------
	
	def allStartingFrom(baseForm: Pokemon): Iterable[EvolveGroup] = {
		baseForm.evolutionsTo.iterator().asScala.filter { _.from == baseForm }.toVector.emptyOneOrMany match {
			// Case: Evolves linearly => Recursively builds the group
			case Some(Left(linear)) => allStartingFrom(linear.to).map { baseForm +: _ }
			// Case: Splits => Forms one linear group and builds splitting branches from the rest
			case Some(Right(splitting)) =>
				val (splittingEvolves, linearEvolves) = splitting.divideBy { _.carryStats }
				linearEvolves.headOption match {
					// Case: Mixture of linear and splitting evolutions => Uses one linear evolution
					case Some(linear) =>
						allStartingFrom(linear.to).map { baseForm +: _ } ++
							(linearEvolves.tail ++ splittingEvolves)
								.flatMap { e => allStartingFrom(e.to).map { _.copy(baseForm = Some(baseForm)) } }
					// Case: Only splitting evolutions => Converts one of them into a linear evolution
					case None =>
						val linearIndex = splitting.indexWhereOption { _.`type`.usesLevel() }
							.getOrElse { RandomSource.nextInt(splitting.size) }
						allStartingFrom(splitting(linearIndex).to).map { baseForm +: _ } ++
							splitting.withoutIndex(linearIndex)
								.flatMap { e => allStartingFrom(e.to).map { _.copy(baseForm = Some(baseForm)) } }
				}
			// Case: Doesn't evolve => Wraps in a group
			case None => Some(singleForm(baseForm))
		}
	}
	
	def singleForm(poke: Pokemon) =
		apply(Vector(poke), megas = Set.from(poke.megaEvolutionsTo.iterator().asScala.map { _.to }))
}

/**
 * Lists the evolutionary chain of a single pokemon type
 * @author Mikko Hilpinen
 * @since 6.7.2023, v1.0-alt
 * @param forms Linear evolve forms of this pokemon.
 *              For secondary forms of splitting evolves (e.g. Nincada -> Shedinja), contains the forms after the split
 * @param baseForm The form that comes before this chain.
 *                 Applicable for secondary splitting evolves, only.
 * @param megas Mega evolution(s) for of the final evolved form, if applicable
 */
// TODO: Handle alternative pokemon forms (should still count as one pokemon / group)
case class EvolveGroup(forms: Vector[Pokemon], baseForm: Option[Pokemon] = None, megas: Set[Pokemon] = Set.empty)
{
	// ATTRIBUTES   ---------------------
	
	lazy val levelThresholds = {
		forms.tail.foldLeftIterator(forms.head -> 0) { case ((lastForm, lastLevel), poke) =>
			// Finds the evolution-link between the two linear forms, if possible
			val evo = poke.evolutionsFrom.asScala.find { _.from.number == lastForm.number }
				.orElse { lastForm.evolutionsTo.asScala.find { _.to.number == poke.number } }
				.orElse { poke.evolutionsFrom.asScala.headOption }
				.orElse { lastForm.evolutionsTo.asScala.headOption }
			// Gets the level threshold from the evolution-link, if possible
			// Otherwise assumes a level range
			val levelThreshold = evo.filter { _.`type`.usesLevel() }.map { _.extraInfo }
				.getOrElse { lastLevel + assumedFormLevelDuration }
			poke -> levelThreshold
		}.toVector
	}
	lazy val levelRanges = levelThresholds.paired.map { case Pair((poke, prev), (_, next)) =>
		poke -> NumericSpan(prev, next - 1)
	} :+ (levelThresholds.last._1 -> NumericSpan(levelThresholds.last._2, 100))
	
	
	// COMPUTED -------------------------
	
	def finalForm = forms.last
	
	def iterator = forms.iterator ++ megas.iterator
	def finalToBaseIterator = forms.reverseIterator
	def megaToBaseIterator = megas.iterator ++ finalToBaseIterator
	
	def canAddSecondaryType(implicit types: Types) = finalToBaseIterator.forall { types(_).secondary.isEmpty }
	def hasSecondaryTypes(implicit types: Types) = megaToBaseIterator.exists { types(_).secondary.nonEmpty }
	
	def types(implicit types: Types) = iterator.flatMap { types(_).types }.toSet
	def primaryTypes(implicit types: Types) = iterator.map { types(_).primary }.toSet
	def secondaryTypes(implicit types: Types) = iterator.flatMap { types(_).secondary }.toSet
	
	
	// OTHER    -------------------------
	
	def formAtLevel(level: Int) = {
		levelThresholds.indexWhereOption { _._2 > level } match {
			case Some(nextIndex) => forms(nextIndex - 1)
			case None => finalForm
		}
	}
	
	def +:(baseForm: Pokemon) = copy(forms = baseForm +: forms, baseForm = None)
}