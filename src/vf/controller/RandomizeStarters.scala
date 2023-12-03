package vf.controller

import com.dabomstew.pkrandom.romhandlers.RomHandler
import utopia.flow.collection.CollectionExtensions._
import utopia.flow.collection.immutable.range.NumericSpan
import utopia.flow.operator.enumeration.End
import vf.model.{EvolveGroup, TypeSet}

import scala.collection.mutable
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
	 * @return Mapping from the original evolve groups to the replacement groups + assigned starter trios
	 */
	def apply(groups: Seq[EvolveGroup], groupByNumber: Map[Int, EvolveGroup])(implicit rom: RomHandler) =
	{
		Log("starters") { writer =>
			val threeFormGroups = groups.filter { _.forms.size == 3 }
			writer.println(s"There are ${threeFormGroups.size} 3 form pokes available")
			// Results are grouped by primary type, then by secondary
			// Secondary and tertiary types are iterated lazily
			val options = threeFormGroups.flatMap { primaryGroup =>
				val primaryForms = primaryGroup.forms.ends
				val primaryFormTypes = primaryForms.map { _.types }
				val primaryFormBst = primaryForms.map { _.bst }
				// writer.println(s"\nStarting from $primaryGroup (${primaryFormTypes.mkString(" -> ")} / ${
				// 	primaryFormBst.mkString(" -> ")} BST)")
				val secondaryOptions = threeFormGroups.iterator.flatMap { secondaryGroup =>
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
								testTypeMatch(secondaryTypes, primaryTypes)
							}
						// Also applies a BST variance limit
						lazy val secondaryIsWithinBstRange = End.values.forall { end =>
							maxBstVarianceRange.contains((secondaryFormBst(end) - primaryFormBst(end)) / primaryFormBst(end))
						}
						if (secondaryHasAdvantage && secondaryIsWithinBstRange) {
							// writer.println(s"\t- When the stronger poke is $secondaryGroup (${
							// 	secondaryFormTypes.mkString(" -> ")} / ${secondaryFormBst.mkString(" -> ") } BST)")
							// Makes sure there are also those options available
							// that are strong against this secondary type and weak against the primary type
							val tertiaryGroupOptions = threeFormGroups.iterator.flatMap { tertiaryGroup =>
								if (tertiaryGroup == primaryGroup || tertiaryGroup == secondaryGroup)
									None
								else {
									val tertiaryForms = tertiaryGroup.forms.ends
									val isPossible = End.values.forall { end =>
										val tertiary = tertiaryForms(end)
										val tertiaryTypes = tertiary.types
										// Again, includes a BST variance check
										lazy val bst = tertiary.bst
										// lazy val causeStart = s"\t\t- ${tertiary.name} (${
										//	tertiary.types} / $bst BST) was rejected because"
										
										if (testTypeMatch(tertiaryTypes, secondaryFormTypes(end))) {
											if (testTypeMatch(primaryFormTypes(end), tertiaryTypes)) {
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
									/*
									if (isPossible) {
										writer.println(s"\t\t- $tertiaryGroup (${
											tertiaryForms.map { _.types }.mkString(" -> ")} / ${
											tertiaryForms.map { _.bst }.mkString(" -> ") } BST)")
									}*/
									if (isPossible)
										Some(tertiaryGroup)
									else
										None
								}
							}.caching
							if (tertiaryGroupOptions.isEmpty)
								None
							else
								Some(secondaryGroup -> tertiaryGroupOptions)
						}
						else
							Iterable.empty
					}
				}.caching
				if (secondaryOptions.isEmpty)
					None
				else
					Some(primaryGroup -> secondaryOptions)
			}
			if (options.isEmpty) {
				println("\nWARNING: No starters are possible")
				writer.println("No starters are possible!")
				Map[EvolveGroup, EvolveGroup]() -> Vector()
			}
			else {
				// Makes sure the number of starters matches the game rules
				val starterCount = rom.starterCount()
				writer.println(s"\nGame starter count is $starterCount")
				// Selects n trios to match the game starter count
				val selectedGroups = mutable.Set[EvolveGroup]()
				val selectedTrios = Iterator.continually {
					// Randomly selects the first option form
					val primaryOptions = options.filterNot { case (primary, secondaries) =>
						selectedGroups.contains(primary) || secondaries.forall { case (secondary, tertiaries) =>
							selectedGroups.contains(secondary) || tertiaries.forall(selectedGroups.contains)
						}
					}
					val (primary, secondaryOptions) = primaryOptions.random
					writer.println(s"\nSelected $primary from ${primaryOptions.size} options")
					// Then selects the second and third forms
					val unusedSecondaryOptions = secondaryOptions.filterNot { case (secondary, tertiaries) =>
						selectedGroups.contains(secondary) || tertiaries.forall(selectedGroups.contains)
					}
					val (secondary, tertiaryOptions) = unusedSecondaryOptions.random
					writer.println(s"Selected $secondary from ${unusedSecondaryOptions.size} options")
					
					val unusedTertiaryOptions = tertiaryOptions.filterNot(selectedGroups.contains)
					val tertiary = unusedTertiaryOptions.random
					writer.println(s"Selected $tertiary from ${unusedTertiaryOptions.size}")
					
					val selected = Vector(primary, secondary, tertiary)
					selectedGroups ++= selected
					selected
				}.take(starterCount / 3).toVector
				// May add some groups if the number of starters is not divisible by three
				val additionalGroups = {
					val remainder = starterCount % 3
					if (remainder == 0)
						Vector()
					else
						Iterator.continually {
							val selected = threeFormGroups.filterNot(selectedGroups.contains).random
							selectedGroups += selected
							selected
						}.take(remainder).toVector
				}
				val appliedStarterGroups = selectedTrios.flatten ++ additionalGroups
				
				// Applies starters to the game
				val originalStarterNumbers = rom.getStarters.iterator().asScala.map { _.number }.toVector
				val javaStarters = appliedStarterGroups.map { _.firstForm.wrapped }.asJava
				rom.setStartersList(javaStarters)
				rom.setStarters(javaStarters)
				
				// Logs the results
				writer.println(s"\nSelected the following trios:")
				selectedTrios.foreach { trio =>
					writer.println(s"\t- ${ trio.map { _.firstForm.name }.mkString(" / ") }")
					trio.zipWithIndex.foreach { case (group, trioIndex) =>
						writer.println(s"\t\t- $group")
						group.forms.zipWithIndex.foreach { case (poke, formIndex) =>
							writer.println(s"\t\t\t- ${poke.name} (${poke.types} / ${poke.bst} BST)")
							val eff = poke.types.effectiveness
							val weaker = trio(if (trioIndex == 0) 2 else trioIndex - 1).forms(formIndex)
							val stronger = trio(if (trioIndex == 2) 0 else trioIndex + 1).forms(formIndex)
							writer.println(s"\t\t\t\t- Type effectiveness against ${ weaker.name }: Attacking = ${
								eff.offenseRatingAgainst(weaker.types) }; Defending = ${
								eff.defenseRatingAgainst(weaker.types) }; Total = ${ eff.against(weaker.types) }")
							writer.println(s"\t\t\t\t- Type effectiveness against ${stronger.name}: Attacking = ${
								eff.offenseRatingAgainst(stronger.types)}; Defending = ${
								eff.defenseRatingAgainst(stronger.types)}; Total = ${eff.against(stronger.types)}")
						}
					}
					/*
					val effectivenessAgainst2 = effectiveness.against(secondaryFormTypes(end))
					if (effectivenessAgainst2.isPositive.isCertainlyTrue) {
						val effectivenessAgainst1 = effectiveness.against(primaryFormTypes(end))
					 */
				}
				if (additionalGroups.nonEmpty) {
					writer.println("\nAlso selected the following additional poke-groups")
					additionalGroups.foreach { group => writer.println(s"\t- $group") }
				}
				
				// Returns a mapping for the original vs. new starters + assigned starter trios
				val starterMapping = originalStarterNumbers.zip(appliedStarterGroups)
					.map { case (originalStarterNumber, newGroup) =>
						val originalGroup = groupByNumber(originalStarterNumber)
						originalGroup -> newGroup
					}
					.toMap
				starterMapping -> selectedTrios
			}
		}
	}
	
	private def testTypeMatch(strongerType: TypeSet, weakerType: TypeSet) = {
		val eff = strongerType.effectiveness
		val defense = eff.defenseRatingAgainst(weakerType)
		val offense = eff.offenseRatingAgainst(weakerType)
		if (defense < 0 || offense < 0)
			false
		else
			defense + offense >= 1.5
	}
}
