package vf.model

import utopia.flow.collection.CollectionExtensions._
import utopia.flow.collection.immutable.Pair
import utopia.flow.collection.immutable.range.NumericSpan
import utopia.flow.util.NotEmpty
import vf.controller.Settings
import vf.model.EvolveGroup.assumedFormLevelDuration

object EvolveGroup
{
	// ATTRIBUTES   ----------------------
	
	private val assumedFormLevelDuration = 15
	
	
	// COMPUTED --------------------------
	
	def all(implicit pokes: Pokes) =
		pokes.iterator.filter { _.isBasicForm }.flatMap(allStartingFrom).toVector
	
	
	// OTHER    --------------------------
	
	def allStartingFrom(baseForm: Poke)(implicit pokes: Pokes): Iterable[EvolveGroup] = {
		baseForm.toEvos.emptyOneOrMany match {
			// Case: Evolves linearly => Recursively builds the group
			case Some(Left(linear)) => appendRoot(baseForm, linear, Vector.empty)
			// Case: Splits => Forms one linear group and builds splitting branches from the rest
			case Some(Right(splitting)) =>
				val (splittingEvolves, linearEvolves) = splitting.divideBy { _.carriesStats }.toTuple
				// Case: Mixture of linear and splitting evolutions => Uses one linear evolution
				if (linearEvolves.nonEmpty) {
					val linear = linearEvolves.random
					appendRoot(baseForm, linear, linearEvolves.filter { _ == linear } ++ splittingEvolves)
				}
				// Case: Only splitting evolutions => Converts one of them into a linear evolution
				else {
					val linear = NotEmpty(splitting.filter { _.usesLevel }).getOrElse(splitting).random
					appendRoot(baseForm, linear, splitting.filterNot { _ == linear })
				}
			// Case: Doesn't evolve => Wraps in a group
			case None => Some(singleForm(baseForm))
		}
	}
	
	def singleForm(poke: Poke)(implicit pokes: Pokes) =
		apply(Vector(poke), megas = Set.from(poke.megaForms))
	
	private def appendRoot(root: Poke, linear: Evo, others: Iterable[Evo])(implicit pokes: Pokes) = {
		val processedOthers = others.flatMap { e => allStartingFrom(e.to).map { e =>
			if (e.baseForm.isDefined) e else e.copy(baseForm = Some(root))
		} }
		val linearBranches = allStartingFrom(linear.to).toVector
		if (linearBranches.isEmpty)
			processedOthers
		else
			(root +: linearBranches.head) +: (linearBranches.tail ++ processedOthers)
	}
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
case class EvolveGroup(forms: Vector[Poke], baseForm: Option[Poke] = None, megas: Set[Poke] = Set.empty)
{
	// ATTRIBUTES   ---------------------
	
	lazy val levelThresholds = {
		forms.tail.foldLeftIterator(forms.head -> 0) { case ((lastForm, lastLevel), poke) =>
			// Finds the evolution-link between the two linear forms, if possible
			val evo = poke.state.evoFrom(lastForm)
				.orElse { lastForm.evoTo(poke.number) }
				.orElse { poke.fromEvos.headOption }
				.orElse { lastForm.toEvos.headOption }
			// Gets the level threshold from the evolution-link, if possible
			// Otherwise assumes a level range
			val levelThreshold = evo.flatMap { _.levelThreshold }.getOrElse { lastLevel + assumedFormLevelDuration }
			poke -> levelThreshold
		}.toVector
	}
	lazy val levelRanges = levelThresholds.paired.map { case Pair((poke, prev), (_, next)) =>
		poke -> NumericSpan(prev, next - 1)
	} :+ (levelThresholds.last._1 -> NumericSpan(levelThresholds.last._2, 100))
	
	/**
	 * All evolutions that are involved in this group/chain. Ordered.
	 */
	lazy val evos = (baseForm.toVector ++ forms).paired
		.foldLeft(Vector[Evo]()) { (evos, formPair) => evos ++ formPair.merge { _ evoTo _ } }
	
	/**
	 * A custom-assigned favouriteness -level for this evolve-group where 0 is not favourite, 1 is preferred, and
	 * > 1 is very favored
	 */
	lazy val favouriteLevel = Settings.favouriteLevelOf(this)
	
	
	// COMPUTED -------------------------
	
	def size = forms.size + megas.size
	
	def firstForm = forms.head
	def finalForm = forms.last
	
	/**
	 * @return An iterator that returns all forms of this poke (including megas) from the lowest to highes
	 */
	def iterator = forms.iterator ++ megas.iterator
	def finalToBaseIterator = forms.reverseIterator
	def megaToBaseIterator = megas.iterator ++ finalToBaseIterator
	
	def canAddSecondaryType = finalToBaseIterator.forall { _.types.isSingleType }
	def hasSecondaryTypes = megaToBaseIterator.exists { _.types.isDualType }
	
	def types = iterator.flatMap { _.types.types }.toSet
	def primaryTypes = iterator.map { _.types.primary}.toSet
	def secondaryTypes = iterator.flatMap { _.types.secondary }.toSet
	
	def canMegaEvolve = megas.nonEmpty
	
	
	// IMPLEMENTED  ---------------------
	
	override def toString = {
		val formsStr = forms.oneOrMany match {
			case Left(only) => only.name
			case Right(forms) => s"${forms.head.name}-${forms.last.name}"
		}
		val baseStr = baseForm match {
			case Some(base) => s" (from ${base.name})"
			case None => ""
		}
		val megaStr = if (megas.nonEmpty) " + megas" else ""
		s"$formsStr$megaStr$baseStr"
	}
	
	
	// OTHER    -------------------------
	
	def formAtLevel(level: Int) = {
		levelThresholds.findIndexWhere { _._2 > level } match {
			case Some(nextIndex) => forms(nextIndex - 1)
			case None => finalForm
		}
	}
	
	def +:(baseForm: Poke) = copy(forms = baseForm +: forms, baseForm = None)
}