package vf.controller

import com.dabomstew.pkrandom.Settings
import com.dabomstew.pkrandom.romhandlers.RomHandler
import utopia.flow.collection.CollectionExtensions._
import utopia.flow.collection.immutable.Pair
import utopia.flow.collection.immutable.range.NumericSpan
import utopia.flow.operator.Identity
import utopia.flow.util.NotEmpty
import vf.model.TypeRelation.{Relative, StrongRelative, WeakRelative}
import vf.model._
import vf.util.RandomUtils._

import java.io.PrintWriter
import scala.collection.mutable
import scala.jdk.CollectionConverters._

/**
 * Used for randomizing the wild pokemon encounters,
 * which also determines the pool of pokemon for the trainer encounters.
 * @author Mikko Hilpinen
 * @since 15.7.2023, v1.0-alt
 */
object RandomizeWildEncounters
{
	// ATTRIBUTES   -------------------------
	
	// How many random pokes should appear for each original poke
	private val pokeToRandomRatio = 2
	
	private val allowedWildBstRange = NumericSpan(0.7, 1.4)
	private val allowedFinalBstRange = NumericSpan(0.6, 1.6)
	private val allowedBstRanges = Pair(allowedWildBstRange, allowedFinalBstRange)
	private val bstDiffWeightMods = Pair(0.5, 0.25)
	
	private val minTypeRelation: TypeRelation = WeakRelative
	private val sameTypeWeight = 8
	private val typeWeights = Map[TypeRelation, Double](StrongRelative -> 4, Relative -> 2, WeakRelative -> 1)
	
	private val baseFormWeight = 2.0
	private val sameGroupWeight = 0.25
	private val singleFormWeight = 3.0
	
	private val weightIncreasePerFavouriteLevel = 1.0
	
	
	// OTHER    -----------------------------
	
	// Returns the poke-mapping and the lowest appearance levels of each poke-group + all encounters
	def all(evolveGroups: Iterable[EvolveGroup])
	       (implicit rom: RomHandler, settings: Settings, pokes: Pokes) =
	{
		Log("wild-encounters") { writer =>
			val levelMod = 1.0 + settings.getWildLevelModifier / 100.0
			writer.println(s"Applied level mod is $levelMod")
			val useTimeOfDay = settings.isUseTimeBasedEncounters
			val baseEncounters = rom.getEncounters(useTimeOfDay)
			val originalEncounters = rom.collapsedEncounters(baseEncounters)
			// Scans all zones, encounters and levels
			val allEncounters = originalEncounters.iterator().asScala.flatMap { encounters =>
				encounters.encounters.iterator().asScala.map { e => new WildEncounter(encounters.offset, e) }
			}.toVector
			// Maps levels, if appropriate
			if (levelMod != 1.0)
				allEncounters.foreach { _.scaleLevelBy(levelMod) }
			val encountersByPokeNumber = allEncounters.groupBy { _.poke.number }
			// FIXME: Likely wrong, because yields levels like 0
			val pokeLevelRanges = encountersByPokeNumber.view.mapValues { encounters =>
				val levels = encounters.map { _.levelRange }
				NumericSpan(levels.iterator.map { _.start }.min, levels.iterator.map { _.end }.max)
			}.toMap
			val encounteredPokes = encountersByPokeNumber.keySet
			// Checks in how many zones each poke appears
			val zoneCountByPoke = encountersByPokeNumber
				.view.mapValues { encounters => encounters.view.map { _.zone }.toSet.size }.toMap
			
			// Groups the pokes into evolve-groups
			val originalGroups = evolveGroups.filter { _.forms.exists { poke => encounteredPokes.contains(poke.number) } }
			
			// Generates n random evolve groups to match each original evolve group
			// Each original poke evo form gets at least one new representation
			val availableGroups = mutable.Set.from(evolveGroups)
			// Keys are poke numbers
			// Values consist of three parts:
			//      1) New mapping evolve-group
			//      2) Mapped specific poke
			//      3) Allowed encounter levels
			val pokeMapping = originalGroups.iterator.flatMap { originalGroup =>
				// Determines the number of representations for each evolve form that appeared in the wild
				// If there are not enough zones (maps) where the poke appears, limits to the number of maps listed
				// E.g. Bagon appears only in a single map, therefore it only gains one match
				val wildForms = originalGroup.forms.filter { p => encounteredPokes.contains(p.number) }
				val zoneCount = wildForms.map { p => zoneCountByPoke.getOrElse(p.number, 0) }.sum
				val formCount = wildForms.size
				val minRepresentationsPerForm = ((pokeToRandomRatio min zoneCount) / formCount) max 1
				val additionalRepresentations = {
					if (formCount >= pokeToRandomRatio)
						0
					else
						(((pokeToRandomRatio min zoneCount) % formCount) min
							(zoneCount - minRepresentationsPerForm * formCount)) max 0
				}
				// Generates matches for each original form separately
				// Each entry contains: 1) Original poke, 2) Selected replacement group,
				//                      3) Selected replacement poke, 4) Level range
				val matches = wildForms.zipWithIndex.flatMap { case (original, index) =>
					val matchCount = minRepresentationsPerForm + (if (index < additionalRepresentations) 1 else 0)
					// Splits the level range up for each appearance, if possible
					val requiredLevelRanges = pokeLevelRanges.get(original.number) match {
						// Case: This form appears in the wild => Determines the level range
						case Some(appearanceRange) =>
							// Case: Matches only one poke => Gives the whole level range to that poke
							if (matchCount == 1)
								Vector(Some(appearanceRange))
							// Case: Matches to more pokes than there are available levels =>
							// Gives the whole level range to all of the matching pokes
							else if (matchCount >= appearanceRange.length)
								Vector.fill(matchCount)(Some(appearanceRange))
							// Case: Possible to distribute the level range => Divides it between the matches
							else {
								val segmentLength = appearanceRange.length / matchCount
								(0 until matchCount)
									.map { i => Some(NumericSpan(i * segmentLength, (i + 1) * segmentLength)) }
							}
						// Case: No appearances in the wild => No level range required
						case None => Vector.fill(matchCount)(None)
					}
					requiredLevelRanges.map { range =>
						val (selectedGroup, selectedPoke) = findMatch(original, originalGroup, range, availableGroups,
							writer)
						// Remembers that this group was used up
						availableGroups -= selectedGroup
						(original, selectedGroup, selectedPoke, range)
					}
				}
				// Forms a map that determines which poke gets mapped to which at which levels
				val rawMapping = originalGroup.forms.map { original =>
					val appliedMatches = NotEmpty(matches.filter { _._1 == original }).getOrElse(matches)
					val firstEncounterLevel = appliedMatches.flatMap { _._4 }.map { _.start }.reduceOption { _ min _ }
					original.number ->
						(appliedMatches.map { case (_, group, poke, levelRange) => (group, poke, levelRange) },
							firstEncounterLevel)
				}
				// Completes the mapping by adding missing level entries (min & max)
				val modifiedMapping = rawMapping.zipWithIndex
					.map { case ((pokeNumber, (matches, firstEncounterLevel)), index) =>
						// Option 1: Use lowest encounter level, if encountered
						val originalLevel = firstEncounterLevel
							// Option 2: Use the lowest encounter level of a previous form,
							.orElse { rawMapping.take(index - 1).reverseIterator.findMap { _._2._2 } }
							// Option 3: Level 0 or lowest evolve level
							.getOrElse(0)
						// Limit to mapped poke minimum evolve level
						val matchesWithLevels = matches.map { case (newGroup, newPoke, encounterLevelRange) =>
							val minLevel = newGroup.levelThresholds.find { _._1 == newPoke } match {
								case Some((_, evolveLevel)) => originalLevel max evolveLevel
								case None => originalLevel
							}
							val actualLevelRange = encounterLevelRange match {
								case Some(range) =>
									if (minLevel > range.end)
										NumericSpan.singleValue(minLevel)
									else if (range.start > minLevel)
										range
									else
										range.withStart(minLevel)
								case None => NumericSpan(minLevel, 100)
							}
							(newGroup, newPoke, actualLevelRange)
						}
						pokeNumber -> matchesWithLevels.sortBy { _._3.start }
					}
				
				// Logs
				writer.println(s"\nDetermining mappings for $originalGroup:")
				writer.println(s"\t- Appears in $formCount forms in the wild: ${
					wildForms.map { _.name }.mkString(", ") }")
				writer.println(s"\t- Appears in $zoneCount maps")
				writer.println(s"\t- Each form gets $minRepresentationsPerForm matches, and $additionalRepresentations additional matches are added as well")
				matches.foreach { case (original, group, poke, levels) =>
					writer.println(s"\t\t- ${original.name} => ${ poke.name } from $group at levels $levels")
				}
				writer.println("\t- Semi-processed version:")
				rawMapping.foreach { case (pkNum, (matches, level)) =>
					writer.println(s"\t\t- $pkNum: ${matches.size} lvl $level+ matches")
					matches.foreach { case (_, poke, _) =>
						writer.println(s"\t\t\t- ${poke.name}")
					}
				}
				writer.println("\t- Final processed version:")
				modifiedMapping.foreach { case (pkNumber, matches) =>
					writer.println(s"\t\t- $pkNumber: ${matches.size} matches:")
					matches.foreach { case (_, poke, lvl) =>
						writer.println(s"\t\t\t- ${poke.name} at levels $lvl")
					}
				}
				
				modifiedMapping
			}.toMap
			
			// Distributes the zones between the random-matched forms, modifying the encounters in the process
			val lowestAppearanceLevels = encountersByPokeNumber.iterator
				// TODO: Move to a separate method and refactor
				.flatMap { case (pokeNumber, encounters) =>
					val matches = pokeMapping(pokeNumber).sortBy { _._3.start }
					val encountersByZone = encounters.groupBy { _.zone }
					// FIXME: minLevelData is wrong (too high levels for first forms, consistently)
					/*
					Alomomola (11 encounters in 11 maps) maps to:
						- Basculin (Water|Ground) from Basculin at levels 0 to 2
						- Seel (Water) from Seel-Dewgong at levels 2 to 4
					Which translates to:
						- Basculin (Water|Ground) from lvl 28 onwards
						- Seel-Dewgong (Water|Psychic) from lvl 33 onwards
					 */
					/*
					Processing 594
						- Distributes between:
							- Basculin at levels 0 to 2
							- Seel at levels 2 to 4
						- 11 zones => 5 per match + 1
							- Level ranges: 28 to 28, 33 to 33, 33 to 33, 33 to 33, 33 to 33, 33 to 33, 33 to 33, 33 to 33, 33 to 33, 33 to 33, 33 to 33
							- Average levels: 28.0, 33.0, 33.0, 33.0, 33.0, 33.0, 33.0, 33.0, 33.0, 33.0, 33.0
						- Basculin
							- Default distribution is zones 1-7
							- Considers 0 of those zones better for the previous match		- Gives zone 1 (levels 28 to 28) to Basculin
							- Gives zone 2 (levels 33 to 33) to Basculin
							- Gives zone 3 (levels 33 to 33) to Basculin
							- Gives zone 4 (levels 33 to 33) to Basculin
							- Gives zone 5 (levels 33 to 33) to Basculin
							- Gives zone 6 (levels 33 to 33) to Basculin
						- Seel
							- Default distribution is zones 7-12
							- Considers 0 of those zones better for the previous match		- Gives zone 1 (levels 33 to 33) to Seel
							- Gives zone 2 (levels 33 to 33) to Seel
							- Gives zone 3 (levels 33 to 33) to Seel
							- Gives zone 4 (levels 33 to 33) to Seel
							- Gives zone 5 (levels 33 to 33) to Seel
					 */
					val minLevelData: Vector[(EvolveGroup, Int)] = {
						val zoneCount = encountersByZone.size
						// Case: Less zones than variants to fit in => Assigns all variants to all available zones
						// (Shouldn't come here because of the match logic limitation above)
						if (zoneCount < matches.size)
							encountersByZone.valuesIterator.flatMap { encounters =>
								encounters.map { encounter =>
									val (selectedGroup, selectedPoke, _) =
										NotEmpty(matches.filter { _._3.overlapsWith(encounter.levelRange) })
											.getOrElse(matches)
											.random
									encounter.poke = selectedPoke
									selectedGroup -> encounter.minLevel
								}
							}.toVector
						// Case: At least one zone available for each variant => Assigns one zone for only one variant
						else {
							// Assigns the zones in leveled order between the forms
							val zonesPerMatch = zoneCount / matches.size
							val additionalZonesCount = zoneCount % matches.size
							
							// FIXME: It seems like either this or the earlier encounter level ranges is wrong -
							//  at least there's a discrepancy
							val zoneLevelRanges = encountersByZone.view
								.mapValues { encounters =>
									val levels = encounters.map { _.levelRange }
									NumericSpan(levels.iterator.map { _.start }.min, levels.iterator.map { _.end }.max)
								}
								.toMap
							val zoneAverageLevels = encountersByZone.view
								.mapValues { encounters =>
									encounters.iterator.map { _.levelRange.ends.sum / 2.0 }.sum / encounters.size
								}
								.toMap
							
							val levelOrderedZones = encountersByZone
								.map { case (zoneId, encounters) =>
									val avgLevel = zoneAverageLevels(zoneId)
									(zoneId, encounters, avgLevel)
								}
								.toVector.sortBy { _._3 }
							
							writer.println(s"Processing $pokeNumber")
							writer.println(s"\t- Distributes between:")
							matches.foreach { case (_, poke, levelRange) =>
								writer.println(s"\t\t- ${poke.name} at levels $levelRange")
							}
							writer.println(s"\t- $zoneCount zones => $zonesPerMatch per match + $additionalZonesCount")
							writer.println(s"\t\t- Level ranges: ${
								zoneLevelRanges.valuesIterator.toVector.sortBy { _.start }.mkString(", ") }")
							writer.println(s"\t\t- Average levels: ${ levelOrderedZones.map { _._3 }.mkString(", ") }")
							
							matches.zipWithIndex.flatMap { case ((group, poke, levels), matchIndex) =>
								// Determines the targeted zone "slice"
								val zonesBefore = {
									// Case: Additional zones don't apply to this match anymore
									if (matchIndex >= additionalZonesCount)
										zonesPerMatch * matchIndex + additionalZonesCount
									// Case: Additional zones still apply
									else
										(zonesPerMatch + 1) * matchIndex
								}
								val numberOfZonesToAssignByDefault =
									if (matchIndex >= additionalZonesCount) zonesPerMatch else zonesPerMatch + 1
								val defaultZones = levelOrderedZones
									.slice(zonesBefore, zonesBefore + numberOfZonesToAssignByDefault)
								// May assign part of the zones to a different match,
								// if the level range is more applicable for that
								val wrongLevelRangeCount = defaultZones.takeWhile { case (zoneId, _, averageLevel) =>
									// Case: First match => Can't move zones to left
									if (matchIndex == 0)
										false
									// Case: Average level is valid
									else if (levels.contains(averageLevel.toInt)) {
										val zoneLevelRange = zoneLevelRanges(zoneId)
										// Case: Whole level range is valid => No need to move the zone
										if (levels.contains(zoneLevelRange.start))
											false
										// Case: Whole level range is not valid => Checks whether moving would help
										else
											matches(matchIndex - 1)._3.contains(zoneLevelRange)
									}
									// Case: Average level is not valid => Checks whether moving would help
									else
										matches(matchIndex - 1)._3.contains(averageLevel.toInt)
								}.size
								
								writer.println(s"\t- ${poke.name}")
								writer.println(s"\t\t- Default distribution is zones ${zonesBefore + 1}-${
									zonesBefore + numberOfZonesToAssignByDefault + 1 }")
								writer.println(s"\t\t- Considers $wrongLevelRangeCount of those zones better for the previous match")
								
								// Performs the actual assignments
								// Always assigns at least one zone to the originally targeted match
								defaultZones.zipWithIndex.map { case ((zoneId, encounters, _), zoneIndex) =>
									val (appliedGroup, appliedPoke, appliedLevels) = {
										// Case: Zone is to be assigned to the previous match
										if (zoneIndex < wrongLevelRangeCount && zoneIndex < defaultZones.size - 1)
											matches(matchIndex - 1)
										// Case: Zone is to be assigned to the current / natural match
										else
											(group, poke, levels)
									}
									val zoneLevelRange = zoneLevelRanges(zoneId)
									// Performs the assignments in correct form
									encounters.foreach { encounter =>
										val poke = {
											if (appliedLevels.contains(zoneLevelRange))
												appliedPoke
											else
												appliedGroup.formAtLevel(zoneLevelRange.start)
										}
										encounter.poke = poke
									}
									writer.println(s"\t\t- Gives zone ${zoneIndex + 1} (levels $zoneLevelRange) to ${
										appliedPoke.name }")
									// Returns the min appearance level
									appliedGroup -> zoneLevelRange.start
								}
							}
						}
					}
					val minLevels = minLevelData.groupMapReduce { _._1 } { _._2 } { _ min _ }
					
					// Logs
					writer.println(s"\n${pokes(pokeNumber).head.name} (${encounters.size} encounters in ${
						encountersByZone.size} maps) maps to:")
					matches.foreach { case (group, poke, levelRange) =>
						writer.println(s"\t- ${poke.name} (${poke.types}) from $group at levels $levelRange")
					}
					writer.println("Which translates to:")
					minLevels.toVector.sortBy { _._2 }.foreach { case (group, minLevel) =>
						writer.println(s"\t- $group (${ group.types.mkString("|") }) from lvl $minLevel onwards")
					}
					
					minLevels
				}
				.toVector.groupMapReduce { _._1 } { _._2 } { _ min _ }
			
			writer.println("\n\nLowest appearance levels:")
			lowestAppearanceLevels.toVector.sortBy { _._2 }.foreach { case (group, level) =>
				writer.println(s"\t- $group: $level")
			}
			
			// Finalizes the change
			allEncounters.foreach { _.setForme() }
			
			// Applies the change
			rom.setEncounters(useTimeOfDay, baseEncounters)
			
			// Forms a new poke-mapping that contains the minimum appearance level information, also
			/*
			val finalizedMapping = pokeMapping.view.mapValues { _.map { case (group, poke, levelRange) =>
				val newLevelRange = lowestAppearanceLevels.get(group) match {
					case Some(lowestAppearanceLevel) =>
						if (levelRange.start > lowestAppearanceLevel)
							levelRange
						else if (levelRange.end < lowestAppearanceLevel)
							NumericSpan.singleValue(lowestAppearanceLevel)
						else
							levelRange.withStart(lowestAppearanceLevel)
					case None => levelRange
				}
				(group, poke, newLevelRange)
			} }.toMap*/
			
			// Returns the poke-mapping and the lowest appearance levels of each poke-group
			// As well as the encounter data
			(pokeMapping.view.mapValues { _.map { _._1 } }.toMap, lowestAppearanceLevels, allEncounters)
		}
	}
	
	private def findMatch(original: Poke, originalGroup: EvolveGroup, targetLevelRange: Option[NumericSpan[Int]],
	                      groups: Iterable[EvolveGroup], writer: PrintWriter)
	                     (implicit rom: RomHandler) =
	{
		// Sometimes the original poke allocations don't make sense (such as using Sunkern at level 52)
		// Applies at least some minimum evolve requirement
		val minEvolveForm = targetLevelRange.map { range => originalGroup.formAtLevel(range.start) }
		val appliedOriginalForm = minEvolveForm.filter { _.originalState.bst > original.originalState.bst }
			.getOrElse(original)
		
		// Requirements:
		//      1) Appears at its first form during the specified level range (preferred),
		//         or at least appears in one form during the specified level range (if applicable)
		//              - Made this condition optional for now
		//      2) Has similar strength during the specified level range
		//      3) Has somewhat similar final form strength (loose)
		//      4) Has similar current type, relative to the original poke's original type
		val originalAndFinalBst = Pair(appliedOriginalForm.originalState.bst, originalGroup.finalForm.originalState.bst)
		lazy val originalType = appliedOriginalForm.originalState.types
		lazy val typeRelations = TypeRelations.of(originalType)
		val options = groups.flatMap { group =>
			// Checks requirement 1
			val forms = targetLevelRange.map { _.ends.map(group.formAtLevel) }
			// Checks requirements 2 & 3
			val form = forms match {
				case Some(forms) => forms.first
				case None => group.finalForm
			}
			val immediateAndFinal = Pair(form, group.finalForm)
			val bstRatios = immediateAndFinal.map { _.bst }.mergeWith(originalAndFinalBst) { _ / _  }
			if (allowedBstRanges.mergeWith(bstRatios) { _.contains(_) }.forall(Identity)) {
				// Checks requirement 4
				val currentType = form.types
				val isSameType = form.types.types.exists { t => originalType.contains(t) }
				lazy val typeRelation = typeRelations(currentType)
				if (isSameType || typeRelation >= minTypeRelation) {
					// Calculates a randomization weight modifier based on the bst differences and type relations
					val groupWeight = if (group == originalGroup) sameGroupWeight else 1.0
					val formWeight = if (forms.isDefined && form == group.forms.head) baseFormWeight else 1.0
					val formCountWeight = if (forms.forall { _.isSymmetricBy { _.number } }) singleFormWeight else 1.0
					val typeWeight = if (isSameType) sameTypeWeight else typeWeights.getOrElse(typeRelation, 1.0)
					val bstWeight = bstRatios.mergeWith(bstDiffWeightMods) { (ratio, weightMod) =>
						math.pow(1 - (1 - ratio).abs, weightMod)
					}.merge { (a, b) => (a + b) / 2.0 }
					val favouriteWeight = 1.0 + group.favouriteLevel * weightIncreasePerFavouriteLevel
					Some((group, form) -> (groupWeight * formWeight * formCountWeight * typeWeight *
						bstWeight * favouriteWeight))
				}
				else
					None
			}
			else
				None
		}
		if (options.isEmpty) {
			println(s"Warning: No randomization options available for: ${appliedOriginalForm.name} at $targetLevelRange")
			writer.println(s"No options available for ${appliedOriginalForm.name} at lvl $targetLevelRange (${
				appliedOriginalForm.originalState.types} ${appliedOriginalForm.originalState.bst} => ${
				originalGroup.finalForm.originalState.bst} BST). Input = ${groups.size} groups")
			writer.println(s"\t- $typeRelations")
			groups.foreach { group =>
				val forms = targetLevelRange.map { _.ends.map(group.formAtLevel) }
				val form = forms match {
					case Some(forms) => forms.first
					case None => group.finalForm
				}
				val immediateAndFinal = Pair(form, group.finalForm)
				val bstRatios = immediateAndFinal.map { _.bst }.mergeWith(originalAndFinalBst) { _ / _ }
				val bstAccepted = allowedBstRanges.mergeWith(bstRatios) { _.contains(_) }.forall(Identity)
				val currentType = form.types
				val isSameType = form.types.types.exists { t => originalType.contains(t) }
				val typeRelation = typeRelations(currentType)
				val typeAccepted = isSameType || typeRelation >= minTypeRelation
				writer.println(s"\t- $group => ${form.name} (${form.types} ${form.bst} BST)")
				writer.println(s"\t\t- ${group.levelThresholds.map { case (poke, level) => s"$level => ${poke.name}" }.mkString(", ")}")
				writer.println(s"\t\t- BST ratios = $bstRatios; Accepted = $bstAccepted")
				writer.println(s"\t\t- Same type = $isSameType; Type relation = $typeRelation; Accepted = $typeAccepted")
			}
			originalGroup -> appliedOriginalForm
		}
		else
			weighedRandom(options)
	}
	
	/*
	boolean abilitiesAreRandomized = settings.getAbilitiesMod() == Settings.AbilitiesMod.RANDOMIZE;
	
			checkPokemonRestrictions();
			List<Pokemon> banned = this.bannedForWildEncounters();
			banned.addAll(this.getBannedFormesForPlayerPokemon());
			if (!abilitiesAreRandomized) {
				List<Pokemon> abilityDependentFormes = getAbilityDependentFormes();
				banned.addAll(abilityDependentFormes);
			}
			if (banIrregularAltFormes) {
				banned.addAll(getIrregularFormes());
			}
	 */
	
	// if (area.displayName.contains("Rock Smash"))
	// area.displayName.contains("Old Rod") || area.displayName.contains("Good Rod") || area.displayName.contains("Super Rod")
	// area.displayName.contains("Grass/Cave") || area.displayName.contains("Long Grass") || area.displayName.contains("Horde")
	// rodGroup.displayName = "Rod Group"
	// grassGroup.displayName = "Grass Group"
	// area.displayName.contains("Shaking")
}
