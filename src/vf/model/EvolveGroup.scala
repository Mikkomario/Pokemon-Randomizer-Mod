package vf.model

import com.dabomstew.pkrandom.pokemon.Pokemon
import com.dabomstew.pkrandom.romhandlers.RomHandler
import com.dabomstew.pkrandom.{RandomSource, RomFunctions}
import utopia.flow.collection.CollectionExtensions._

import scala.jdk.CollectionConverters._

object EvolveGroup
{
	// COMPUTED --------------------------
	
	def all(implicit rom: RomHandler) =
		RomFunctions.getBasicPokemon(rom).iterator().asScala.flatMap(allStartingFrom).toSet
	
	
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
						val linearIndex = RandomSource.nextInt(splitting.size)
						allStartingFrom(splitting(linearIndex).to).map { baseForm +: _ } ++
							splitting.withoutIndex(linearIndex)
								.flatMap { e => allStartingFrom(e.to).map { _.copy(baseForm = Some(baseForm)) } }
				}
			// Case: Doesn't evolve => Wraps in a group
			case None => Some(singleForm(baseForm))
		}
	}
	
	def singleForm(poke: Pokemon) =
		apply(Vector(poke), mega = poke.megaEvolutionsTo.iterator().asScala.nextOption().map { _.to })
}

/**
 * Lists the evolutionary chain of a single pokemon type
 * @author Mikko Hilpinen
 * @since 6.7.2023, v1.0-alt
 * @param forms Linear evolve forms of this pokemon.
 *              For secondary forms of splitting evolves (e.g. Nincada -> Shedinja), contains the forms after the split
 * @param baseForm The form that comes before this chain.
 *                 Applicable for secondary splitting evolves, only.
 * @param mega Mega for of the final evolved form, if applicable
 */
case class EvolveGroup(forms: Vector[Pokemon], baseForm: Option[Pokemon] = None, mega: Option[Pokemon] = None)
{
	// COMPUTED -------------------------
	
	def finalForm = forms.last
	def finalOrMegaForm = mega.getOrElse(finalForm)
	
	def iterator = forms.iterator ++ mega.iterator
	def finalToBaseIterator = forms.reverseIterator
	def megaToBaseIterator = mega.iterator ++ finalToBaseIterator
	
	def canAddSecondaryType(implicit types: Types) = finalToBaseIterator.forall { types(_).secondary.isEmpty }
	def hasSecondaryTypes(implicit types: Types) = megaToBaseIterator.exists { types(_).secondary.nonEmpty }
	
	def types(implicit types: Types) = iterator.flatMap { types(_).types }.toSet
	def primaryTypes(implicit types: Types) = iterator.map { types(_).primary }.toSet
	def secondaryTypes(implicit types: Types) = iterator.flatMap { types(_).secondary }.toSet
	
	
	// OTHER    -------------------------
	
	def +:(baseForm: Pokemon) = copy(forms = baseForm +: forms, baseForm = None)
}