package vf.controller

import com.dabomstew.pkrandom.romhandlers.RomHandler
import utopia.flow.collection.CollectionExtensions._
import utopia.flow.collection.immutable.range.NumericSpan
import utopia.flow.collection.mutable.iterator.OptionsIterator
import utopia.flow.operator.enumeration.End
import vf.model.EvolveGroup

import scala.jdk.CollectionConverters._

/**
 * Randomizes the starter pokes. Also used for determining rival starter mapping.
 * @author Mikko Hilpinen
 * @since 28.11.2023, v1.0-alt
 */
object RandomizeStarters
{
	private val maxBstVarianceRange = NumericSpan(-0.25, 0.5)
	
	/**
	 * Randomizes the starters in the game
	 * @param groups All available evolve-groups
	 * @param rom Implicit ROM
	 * @return Mapping from the original evolve groups to the replacement groups
	 */
	def apply(groups: Seq[EvolveGroup], groupByNumber: Map[Int, EvolveGroup])(implicit rom: RomHandler) =
	{
		Log("starters") { writer =>
			val threeFormGroups = groups.filter { _.forms.size == 3 }
			writer.println(s"There are ${threeFormGroups.size} 3 form pokes available")
			val options = threeFormGroups.flatMap { primaryGroup =>
				val primaryForms = primaryGroup.forms.ends
				val primaryFormTypes = primaryForms.map { _.types }
				val primaryFormBst = primaryForms.map { _.bst }
				writer.println(s"\nStarting from $primaryGroup (${primaryFormTypes.mkString(" -> ")} / ${
					primaryFormBst.mkString(" -> ")} BST)")
				threeFormGroups.flatMap { secondaryGroup =>
					if (secondaryGroup == primaryGroup)
						Iterable.empty
					else {
						val secondaryForms = secondaryGroup.forms.ends
						val secondaryFormTypes = secondaryForms.map { _.types }
						lazy val secondaryFormBst = secondaryForms.map { _.bst }
						// Only accepts stronger types as secondary types
						// Must apply both in basic and final form
						val secondaryHasAdvantage = secondaryFormTypes
							.forallWith(primaryFormTypes) { (secondaryTypes, primaryTypes) =>
								secondaryTypes.effectiveness.against(primaryTypes).isPositive.isCertainlyTrue
							}
						// Also applies a BST variance limit
						lazy val secondaryIsWithinBstRange = End.values.forall { end =>
							maxBstVarianceRange.contains((secondaryFormBst(end) - primaryFormBst(end)) / primaryFormBst(end))
						}
						if (secondaryHasAdvantage && secondaryIsWithinBstRange) {
							writer.println(s"\t- When the stronger poke is $secondaryGroup (${
								secondaryFormTypes.mkString(" -> ")} / ${secondaryFormBst.mkString(" -> ") } BST)")
							// Makes sure there are also those options available
							// that are strong against this secondary type and weak against the primary type
							threeFormGroups.filter { tertiaryGroup =>
								if (tertiaryGroup == primaryGroup || tertiaryGroup == secondaryGroup)
									false
								else {
									val tertiaryForms = tertiaryGroup.forms.ends
									val isPossible = End.values.forall { end =>
										lazy val tertiary = tertiaryForms(end)
										val effectiveness = tertiary.types.effectiveness
										// Again, includes a BST variance check
										lazy val bst = tertiary.bst
										lazy val causeStart = s"\t\t- ${tertiary.name} (${
											tertiary.types} / $bst BST) was rejected because"
										
										val effectivenessAgainst2 = effectiveness.against(secondaryFormTypes(end))
										if (effectivenessAgainst2.isPositive.isCertainlyTrue) {
											val effectivenessAgainst1 = effectiveness.against(primaryFormTypes(end))
											if (effectivenessAgainst1.isNegative.isCertainlyTrue) {
												val bstVariance1 = (bst - primaryFormBst(end)) / primaryFormBst(end)
												if (maxBstVarianceRange.contains(bstVariance1)) {
													val bstVariance2 = (bst - secondaryFormBst(end)) / secondaryFormBst(end)
													if (maxBstVarianceRange.contains(bstVariance2))
														true
													else {
														// writer.println(s"$causeStart BST varies too much from ${
														//	secondaryForms(end).name} ($bstVariance2)")
														false
													}
												}
												else {
													// writer.println(s"$causeStart BST varies too much from ${
													//	primaryForms(end).name} ($bstVariance1)")
													false
												}
											}
											else {
												// writer.println(s"$causeStart is not weak against ${
												//	primaryForms(end).name} ($effectivenessAgainst1)")
												false
											}
										}
										else {
											// writer.println(s"$causeStart is not strong against ${
											//	secondaryForms(end).name} ($effectivenessAgainst2)")
											false
										}
									}
									if (isPossible) {
										writer.println(s"\t\t- $tertiaryGroup (${
											tertiaryForms.map { _.types }.mkString(" -> ")} / ${
											tertiaryForms.map { _.bst }.mkString(" -> ") } BST)")
									}
									isPossible
								}
							}.map { Vector(primaryGroup, secondaryGroup, _) }
						}
						else
							Iterable.empty
					}
				}
			}
			if (options.isEmpty) {
				println("\nWARNING: No starters are possible")
				writer.println("No starters are possible!")
				Map[EvolveGroup, EvolveGroup]()
			}
			else {
				val selected = options.random
				
				// Makes sure the number of starters matches the game rules
				val starterCount = rom.starterCount()
				writer.println(s"\nGame starter count is $starterCount")
				val correctNumberOfStarters = {
					if (starterCount == 3)
						selected
					else if (starterCount < 3)
						selected.take(starterCount)
					else
						selected ++ OptionsIterator
							.continually { Some(threeFormGroups.random).filterNot { selected.contains } }
							.collectNext(starterCount - 3)
				}
				
				// Applies starters to the game
				val originalStarterNumbers = rom.getStarters.iterator().asScala.map { _.number }.toVector
				val javaStarters = correctNumberOfStarters.map { _.firstForm.wrapped }.asJava
				rom.setStartersList(javaStarters)
				rom.setStarters(javaStarters)
				
				writer.println(s"\nSelected ${correctNumberOfStarters.mkString("/")} from ${options.size} options")
				writer.println("\nAll options were:")
				options.foreach { pokes => writer.println(s"\t- ${pokes.map { _.firstForm.name }.mkString(" / ")}") }
				
				// Returns a mapping for the original vs. new starters
				originalStarterNumbers.zip(correctNumberOfStarters).map { case (originalStarterNumber, newGroup) =>
					val originalGroup = groupByNumber(originalStarterNumber)
					originalGroup -> newGroup
				}.toMap
			}
		}
	}
}
