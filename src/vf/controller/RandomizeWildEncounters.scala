package vf.controller

import com.dabomstew.pkrandom.Settings
import com.dabomstew.pkrandom.romhandlers.RomHandler
import utopia.flow.collection.CollectionExtensions._
import utopia.flow.collection.immutable.Pair
import utopia.flow.collection.immutable.range.{NumericSpan, Span}
import utopia.flow.operator.Identity
import utopia.flow.util.NotEmpty
import vf.model.TypeRelation.{Relative, StrongRelative, WeakRelative}
import vf.model.{EvolveGroup, Poke, Pokes, TypeRelation, TypeRelations, WildEncounter}
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
			val pokeLevelRanges = allEncounters.groupMapReduce { _.poke.number } { _.levelRange } {
				(a, b) => Span.numeric(a.start min b.start, a.end max b.end)
			}
			val encounteredPokes = pokeLevelRanges.keySet
			// Checks in how many zones each poke appears
			val zoneCountByPoke = allEncounters.groupBy { _.poke.number }
				.view.mapValues { encounters => encounters.view.map { _.zone }.toSet.size }.toMap
			
			// Groups the pokes into evolve-groups
			val originalGroups = evolveGroups.filter { _.forms.exists { poke => encounteredPokes.contains(poke.number) } }
			
			// Generates n random evolve groups to match each original evolve group
			// Each original poke evo form gets at least one new representation
			val availableGroups = mutable.Set.from(evolveGroups)
			val pokeMapping = originalGroups.iterator.flatMap { originalGroup =>
				// Determines the number of representations for each evolve form that appeared in the wild
				// If there are not enough zones (maps) where the poke appears, limits to the number of maps listed
				// E.g. Bagon appears only in a single map, therefore it only gains one match
				val wildForms = originalGroup.forms.filter { p => encounteredPokes.contains(p.number) }
				val zoneCount = wildForms.map { p => zoneCountByPoke.getOrElse(p.number, 0) }.sum
				val formCount = wildForms.size
				val minRepresentationsPerForm = ((pokeToRandomRatio / formCount) min zoneCount) max 1
				val additionalRepresentations =
					((if (formCount >= pokeToRandomRatio) 0 else pokeToRandomRatio % formCount) min (zoneCount - minRepresentationsPerForm * formCount)) max 0
				// Generates matches for each original form separately
				val matches = wildForms.zipWithIndex.flatMap { case (original, index) =>
					val matchCount = minRepresentationsPerForm + (if (index < additionalRepresentations) 1 else 0)
					// Splits the level range up for each appearance, if possible
					val requiredLevelRanges = pokeLevelRanges.get(original.number) match {
						// Case: This form appears in the wild => Determines the level range
						case Some(appearanceRange) =>
							if (matchCount == 1)
								Vector(Some(appearanceRange))
							else if (matchCount >= appearanceRange.length)
								Vector.fill(matchCount)(Some(appearanceRange))
							else {
								val segmentLength = appearanceRange.length / matchCount
								(0 until matchCount)
									.map { i => Some(NumericSpan(i * segmentLength, (i + 1) * segmentLength)) }
							}
						// Case: No appearances in the wild => No level range required
						case None => Vector.fill(matchCount)(None)
					}
					requiredLevelRanges.map { range =>
						val representation = findMatch(original, originalGroup, range, availableGroups, writer)
						// Remembers that this group was used up
						availableGroups -= representation._1
						representation -> range
					}
				}
				originalGroup.forms.map { p => p.number -> matches }
			}.toMap
			
			// Distributes the zones between the random-matched forms, modifying the encounters in the process
			val lowestAppearanceLevels = allEncounters.groupBy { _.poke.number }.iterator
				// TODO: Move to a separate method and refactor
				.flatMap { case (pokeNumber, encounters) =>
					val matches = pokeMapping(pokeNumber)
					val encountersByZone = encounters.groupBy { _.zone }
					val minLevels: Vector[(EvolveGroup, Int)] = {
						// Case: Less zones than variants to fit in => Assigns all variants to all available zones
						// (Shouldn't come here because of the match logic limitation above)
						if (encountersByZone.size < matches.size) {
							encountersByZone.valuesIterator.flatMap { encounters =>
								val naturalResults = encounters.map { encounter =>
									val options =
										NotEmpty(matches.filter { _._2.forall { _ overlapsWith encounter.levelRange } })
											.getOrElse(matches)
									val (selectedGroup, selectedPoke) = randomFrom(options)._1
									encounter.poke = selectedPoke
									selectedGroup -> encounter.minLevel
								}
								// Makes sure each appearance gets at least one encounter
								// TODO: There's still a chance that all pokes won't appear (unlikely, however)
								val forcedAdditions = matches
									.filterNot { case ((group, _), _) => naturalResults.exists { _._1 == group } }
									.map { case ((group, poke), _) =>
										val encounter = randomFrom(encounters)
										encounter.poke = poke
										group -> encounter.minLevel
									}
								naturalResults ++ forcedAdditions
							}.toVector
						}
						// Case: At least one zone available for each variant => Assigns one zone for only one variant
						else {
							// Assigns the zones in leveled order between the forms
							val zoneLevelRanges = encountersByZone.view.mapValues { encounters =>
								val levels = encounters.map { _.levelRange }
								Span(levels.iterator.map { _.start }.min, levels.iterator.map { _.end }.max)
							}.toMap
							val zoneAverageLevels = encountersByZone.view.mapValues { encounters =>
								encounters.iterator.map { _.levelRange.ends.sum / 2 }.sum / encounters.size
							}.toMap
							val naturalResults = encountersByZone.iterator.map { case (zoneId, encounters) =>
								val avgLevel = zoneAverageLevels(zoneId)
								val levelRange = zoneLevelRanges(zoneId)
								val options = matches.bestMatch(
									_._2.forall { _.contains(avgLevel) },
									_._2.forall { _.contains(levelRange) })
								val ((selectedGroup, selectedPoke), selectedLevelRange) = randomFrom(options)
								encounters.foreach { encounter =>
									val poke = {
										if (selectedLevelRange.forall { _.contains(levelRange) })
											selectedPoke
										else
											selectedGroup.formAtLevel(levelRange.start)
									}
									encounter.poke = poke
								}
								selectedGroup -> levelRange.start
							}.toVector
							// TODO: Make sure each appearance gets at least one zone
							naturalResults
						}
					}
					
					// Logs
					writer.println(s"\n${pokes(pokeNumber).head.name} (${encounters.size} encounters in ${
						encountersByZone.size} maps) maps to:")
					matches.foreach { case ((group, poke), levelRange) =>
						writer.println(s"\t- ${poke.name} (${poke.types}) from $group at levels $levelRange")
					}
					
					minLevels.groupMapReduce { _._1 } { _._2 } { _ min _ }
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
			
			// Returns the poke-mapping and the lowest appearance levels of each poke-group
			// As well as the encounter data
			(pokeMapping.view.mapValues { _.map { _._1._1 } }.toMap, lowestAppearanceLevels, allEncounters)
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
