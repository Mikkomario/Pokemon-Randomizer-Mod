package vf.controller

import com.dabomstew.pkrandom.pokemon.Trainer
import com.dabomstew.pkrandom.romhandlers.RomHandler
import com.dabomstew.pkrandom.{RandomSource, Settings}
import utopia.flow.collection.CollectionExtensions._
import utopia.flow.collection.immutable.range.Span
import utopia.flow.operator.Identity
import utopia.flow.util.NotEmpty
import utopia.flow.view.mutable.eventful.Flag
import vf.model.TypeRelation.{Relative, StrongRelative, Unrelated, WeakRelative}
import vf.model._
import vf.util.RandomUtils._

import java.io.PrintWriter
import scala.collection.mutable
import scala.jdk.CollectionConverters._
import scala.util.Random

/**
 * Randomizes the pokes used by trainers
 * @author Mikko Hilpinen
 * @since 19.7.2023, v1.0-alt
 */
object RandomizeBattles
{
	// ATTRIBUTES   ----------------------
	
	private val bstRange = Span.numeric(0.85, 1.25)
	
	private val defaultMinTypeRelation: TypeRelation = WeakRelative
	private val minStrictTypeRelation: TypeRelation = StrongRelative
	
	private val bstDiffWeightMod = 0.5
	private val sameTypeWeight = 11.0
	private val typeWeights = Map[TypeRelation, Double](
		StrongRelative -> 8.0, Relative -> 6.0, WeakRelative -> 3.0, Unrelated -> 1.5
	)
	
	
	// OTHER    --------------------------
	
	def all(allGroups: Map[Int, EvolveGroup], starterMapping: Map[EvolveGroup, EvolveGroup],
	        pokeMapping: Map[Int, Vector[EvolveGroup]], minAppearanceLevels: Map[EvolveGroup, Int])
	       (implicit rom: RomHandler, settings: Settings) =
	{
		val levelMod = 1 + settings.getTrainersLevelModifier / 100.0
		// Indices poke groups by poke number. Only includes poke groups that appear in the 'minAppearanceLevels'
		// (i.e. the wild)
		val pokePool = minAppearanceLevels.keySet
		val trainers = rom.getTrainers
		val encounterCounts = Log("trainers") { writer =>
			writer.println(s"Applied level mod is $levelMod")
			val encounterCounts = trainers.iterator().asScala
				// This is the first rival in Yellow. His Pokemon is used to determine the non-player
				// starter, so we can't change it here. Just skip it.
				.filterNot { t => Option(t.tag).contains("IRIVAL") }
				// In this process, collects the encountered pokes from every trainer
				.flatMap { trainer =>
					apply(trainer, levelMod, starterMapping, pokeMapping, pokePool, allGroups, minAppearanceLevels,
						writer)
				}
				// Calculates the number of encounters for each poke
				.toVector.groupMapReduce(Identity) { _ => 1 } { _ + _ }
			
			writer.println(s"\nTotal of ${encounterCounts.size} encountered pokes:")
			encounterCounts.toVector.reverseSortBy { _._2 }.foreach { case (poke, encounters) =>
				writer.println(s"\t- ${poke.name} (${poke.types} / ${poke.bst} BST): $encounters encounters")
			}
			
			encounterCounts
		}
		// TODO: Is double battle mode correct?
		// Saves the changes to the ROM
		rom.setTrainers(trainers, false)
		
		// Returns the number of encounters per poke
		encounterCounts
	}
	
	// Returns the selected pokes
	def apply(trainer: Trainer, levelMod: Double, starterMapping: Map[EvolveGroup, EvolveGroup],
	          pokeMapping: Map[Int, Vector[EvolveGroup]], pokePool: Iterable[EvolveGroup], groups: Map[Int, EvolveGroup],
	          minAppearanceLevels: Map[EvolveGroup, Int], writer: PrintWriter)
	         (implicit rom: RomHandler, settings: Settings) =
	{
		writer.println(s"\n${trainer.name}/${trainer.tag}")
		// Gym trainers, elite four, champion and others use stricter type-matching
		lazy val useStrictTyping = Option(trainer.tag).exists { group =>
			group.startsWith("GYM") || group.startsWith("ELITE") || group.startsWith("CHAMPION") ||
				group.startsWith("THEMED") || group.startsWith("GIO")
		}
		// Finds a match for each of the trainer's original pokes using the following rules:
		//      - Looks for similar strength (bst) level (compared to the original)
		//      - Looks for similar type (relative to the original poke type)
		//          - This requirement is stricter for gym trainers, elite four and champion
		//      - Don't select duplicate pokes
		//      - Prefers direct 1 to n poke-mapping result
		//      - Won't select poke's that don't appear in the game at that point (level) yet
		// Applies the randomization in random order
		val selectedGroups = mutable.Set[EvolveGroup]()
		// Can only apply one mega evo per trainer
		val megaEvoFlag = Flag()
		// Each entry contains: 1) Selected evolve group, 2) Selected poke and 3) Encounter level
		val selectedOpponents = Vector.from(Random.shuffle(trainer.pokemon.asScala).map { tp =>
			val originalNumber = tp.pokemon.number
			val originalGroup = groups(originalNumber)
			val level = math.round(tp.level * levelMod).toInt
			val (selectedGroup, selectedPoke) = starterMapping.get(originalGroup) match {
				// Case: Replacing a starter => Uses a pre-fixed mapping
				case Some(newStarterGroup) => newStarterGroup -> newStarterGroup.formAtLevel(level)
				// Case: Replacing a non-starter
				case None =>
					// If the original poke can mega-evolve, requires the resulting poke to be able to do that as well
					val requireMegaEvolvable = tp.canMegaEvolve
					// Looks for a mapping result first
					// TODO: Apply stricter type-checking here also?
					val mappingOptions = pokeMapping.getOrElse(originalNumber, Vector())
						.filter { g =>
							(!requireMegaEvolvable || g.canMegaEvolve) &&
								minAppearanceLevels.get(g).forall { _ <= level } && !selectedGroups.contains(g)
						}
					// Case: Mapping result available => Selects one randomly
					if (mappingOptions.nonEmpty) {
						val group = randomFrom(mappingOptions)
						group -> group.formAtLevel(level)
					}
					// Case: No mapping result available => Selects randomly from the pool
					else
						findMatchFor(originalGroup, level, pokePool, selectedGroups,
							minAppearanceLevels, writer, useStrictTyping, requireMegaEvolvable)
			}
			selectedGroups += selectedGroup
			
			// Assigns the selected poke
			// TODO: Apply random form
			
			// Logs
			val originalPoke = originalGroup.formAtLevel(tp.level)
			writer.println(s"\t- ${ originalPoke.name } (${ originalPoke.originalState.types }) lvl ${ tp.level } (${
				originalPoke.originalState.bst}) => ${
				selectedPoke.name } (${ selectedPoke.types }) lvl $level (${ selectedPoke.bst })")
			
			tp.pokemon = selectedPoke.randomForm
			tp.level = level
			tp.abilitySlot = selectedPoke.abilitySlots.random
			tp.resetMoves = true
			
			// Enables mega evolution for all pokes that support it
			NotEmpty(selectedPoke.toMegaEvos.filter { _.method == 1 }).foreach { megaEvos =>
				if (megaEvoFlag.set()) {
					writer.println { "\t\t- Mega evolves" }
					val megaEvo = megaEvos.random
					tp.heldItem = megaEvo.argument
				}
			}
			
			// Applies shiny chance (optional)
			if (settings.isShinyChance && RandomSource.nextInt(256) == 0)
				tp.IVs |= (1 << 30)
			
			// Returns selected group, selected poke and applied level
			(selectedGroup, selectedPoke, level)
		})
		
		// Adds additional trainer pokemon, if applicable (settings-based)
		val defaultAdditionalPokeCount = {
			if (trainer.isBoss)
				settings.getAdditionalBossTrainerPokemon
			else if (trainer.isImportant)
				settings.getAdditionalImportantTrainerPokemon
			else
				settings.getAdditionalRegularTrainerPokemon
		}
		// If a trainer can appear in a Multi Battle (i.e., a Double Battle where the enemy consists
		// of two independent trainers), we want to be aware of that so we don't give them a team of
		// six Pokemon and have a 6v12 battle
		val additionalPokeCount = {
			val maxTotalCount = if (trainer.multiBattleStatus != Trainer.MultiBattleStatus.NEVER) 3 else 6
			defaultAdditionalPokeCount min (maxTotalCount - selectedOpponents.size)
		}
		val trainerPokes = selectedOpponents.map { _._2 }
		if (additionalPokeCount > 0) {
			val (trainerPokeGroups, trainerPokeLevels) = selectedOpponents
				.splitMap { case (group, _, level) => (group, level) }
			val (additionalPokes, additionalPokeLevel) = selectAdditionalPokes(additionalPokeCount, trainerPokeGroups,
				trainerPokeLevels, pokePool, selectedGroups, minAppearanceLevels, writer)
			
			// Logs
			writer.println(s"Assigns $additionalPokeCount new pokes:")
			additionalPokes.foreach { poke => writer.println(s"\t- ${poke.name} (${poke.types} ${poke.bst} BST)") }
			
			// We want to preserve the original last Pokemon because the order is sometimes used to
			// determine the rival's starter
			val secondToLastIndex = selectedOpponents.size - 1
			val model = trainer.pokemon.get(0)
			additionalPokes.foreach { poke =>
				val tp = model.copy()
				// WET WET
				tp.pokemon = poke.randomForm
				tp.level = additionalPokeLevel
				tp.abilitySlot = poke.abilitySlots.random
				tp.resetMoves = true
				// Clear out the held item because we only want one Pokemon with a mega stone if we're
				// swapping mega evolvables
				tp.heldItem = 0
				
				trainer.pokemon.add(secondToLastIndex, tp)
			}
			
			trainerPokes ++ additionalPokes
		}
		trainerPokes
	}
	
	private def findMatchFor(originalGroup: EvolveGroup, level: Int, pool: Iterable[EvolveGroup],
	                         used: mutable.Set[EvolveGroup], minAppearanceLevels: Map[EvolveGroup, Int],
	                         writer: PrintWriter,
	                         useStrictTyping: Boolean, requireMegaEvolvable: Boolean)
	                        (implicit rom: RomHandler) =
	{
		// Makes sure the original poke is fully evolved in the original context in order to avoid strange mismatches
		val original = originalGroup.formAtLevel(level)
		
		// Applies the following filters:
		//      1) Min appearance level filter - Will be looser for battles with < lvl 5 pokes
		//      2) Non-duplicate filter
		//      3) BST filter
		//      4) Mega filter
		lazy val originalBst = original.originalState.bst
		lazy val allowedBst = bstRange.mapEnds { _ * originalBst }
		lazy val originalType = original.originalState.types
		lazy val typeRelations = TypeRelations.of(originalType)
		lazy val minTypeRelation = if (useStrictTyping) minStrictTypeRelation else defaultMinTypeRelation
		
		val options = pool.flatMap { group =>
			// 1, 2 & 4
			if ((!requireMegaEvolvable || group.canMegaEvolve) &&
				minAppearanceLevels.get(group).forall { l => l <= level || l <= 5 } && !used.contains(group))
			{
				val form = group.formAtLevel(level)
				val bst = form.bst
				// 3
				if (allowedBst.contains(bst)) {
					// May apply type-filtering also
					val isSameType = originalType.types.exists { t => form.types.contains(t) }
					lazy val typeRelation = typeRelations(form.types)
					if (isSameType || typeRelation >= minTypeRelation) {
						// Calculates a weight modifier based on BST and type
						val bstRatio = bst / originalBst
						val bstWeight = math.pow(1 - (1 - bstRatio).abs, bstDiffWeightMod)
						val typeWeight = if (isSameType) sameTypeWeight else typeWeights.getOrElse(typeRelation, 1.0)
						Some((group, form) -> (bstWeight * typeWeight))
					}
					else
						None
				}
				else
					None
			}
			else
				None
		}
		
		// Selects a random poke from the options
		if (options.isEmpty) {
			println(s"Warning: No options for $originalGroup/${
				original.name} at lvl $level. Strict = $useStrictTyping, Mega = $requireMegaEvolvable")
			
			writer.println(s"No options for $originalGroup/${original.name} at lvl $level ($originalType $originalBst BST). Strict = $useStrictTyping, Mega = $requireMegaEvolvable")
			writer.print(s"\t- Allowed BST range = $allowedBst")
			writer.println(s"\t- $typeRelations")
			pool.foreach { group =>
				val megaAccepted = !requireMegaEvolvable || group.canMegaEvolve
				val minLevel = minAppearanceLevels.get(group)
				val minLevelAccepted = minLevel.forall { _ <= level }
				val wasUsed = used.contains(group)
				val form = group.formAtLevel(level)
				val bst = form.bst
				val bstAccepted = allowedBst.contains(bst)
				val isSameType = originalType.types.exists { t => form.types.contains(t) }
				val typeRelation = typeRelations(form.types)
				val typeAccepted = isSameType || typeRelation >= minTypeRelation
				writer.println(s"\t\t- $group => ${form.name} (${form.types} $bst BST)")
				writer.println(s"\t\t\t- Can mega evolve = ${group.canMegaEvolve}; Mega condition accepted = $megaAccepted")
				writer.println(s"\t\t\t- Min appearance level = $minLevel; Level accepted = $minLevelAccepted")
				writer.println(s"\t\t\t- Was already used = $wasUsed")
				writer.println(s"\t\t\t- BST accepted = $bstAccepted")
				writer.println(s"\t\t\t- Same type = $isSameType; Type relation = $typeRelation; Type accepted = $typeAccepted")
			}
			
			originalGroup -> original
		}
		else
			weighedRandom(options)
	}
	
	// WET WET - Needs refactoring with findMatch
	// Assumes additionalPokeCount > 0
	private def selectAdditionalPokes(additionalPokeCount: Int, originalGroups: Iterable[EvolveGroup],
	                                levels: Iterable[Int], pool: Iterable[EvolveGroup], used: mutable.Set[EvolveGroup],
	                                minAppearanceLevels: Map[EvolveGroup, Int], writer: PrintWriter)
	                               (implicit rom: RomHandler) =
	{
		// Compares against the average BST and uses an average level
		val level = (levels.sum.toDouble / levels.size).round.toInt
		val referenceBst = originalGroups.map { _.formAtLevel(level).originalState.bst }.sum / originalGroups.size
		// Checks whether there is a common type(s) between the original pokes
		// If so, uses a poke that involves one of those types
		val commonTypes = originalGroups.map { _.types }.reduce { _ & _ }
		
		// Applies the following filters:
		//      1) Min appearance level filter - Will be looser for battles with < lvl 5 pokes
		//      2) Non-duplicate filter
		//      3) BST filter
		//      4) Type filter, if applicable
		lazy val allowedBst = bstRange.mapEnds { _ * referenceBst }
		val options = pool.flatMap { group =>
			// 1, 2
			if (minAppearanceLevels.get(group).forall { l => l <= level || l <= 5 } && !used.contains(group)) {
				val form = group.formAtLevel(level)
				val bst = form.bst
				// 3 & 4
				if (allowedBst.contains(bst) && commonTypes.isEmpty || form.types.types.exists(commonTypes.contains)) {
					// Calculates a weight modifier based on BST and type
					val bstRatio = bst / referenceBst
					val bstWeight = math.pow(1 - (1 - bstRatio).abs, bstDiffWeightMod)
					Some(form -> bstWeight)
				}
				else
					None
			}
			else
				None
		}
		
		// Selects a random poke from the options
		if (options.hasSize <= additionalPokeCount) {
			println(s"Warning: Only ${options.size} options for additional pokes at lvl $level. Common types = ${ commonTypes.mkString(", ") }")
			options.map { _._1 } -> level
		}
		else if (additionalPokeCount == 1)
			Vector(weighedRandom(options)) -> level
		else {
			val optionsBuffer = options.toBuffer
			Iterator.continually {
				val selected = weighedRandom(optionsBuffer)
				optionsBuffer.remove(optionsBuffer.indexWhere { _._1.number == selected.number })
				selected
			}.take(additionalPokeCount).toVector -> level
		}
	}
	
	
	/*
	boolean includeFormes = settings.isAllowTrainerAlternateFormes();
			boolean banIrregularAltFormes = settings.isBanIrregularAltFormes();
			boolean swapMegaEvos = settings.isSwapTrainerMegaEvos();
	 */
	//  boolean rivalCarriesStarter = settings.isRivalCarriesStarterThroughout();
	/*
	// Construct groupings for types
				// Anything starting with GYM or ELITE or CHAMPION is a group
				Map<String, List<Trainer>> groups = new TreeMap<>();
				for (Trainer t : currentTrainers) {
					if (t.tag != null && t.tag.equals("IRIVAL")) {
						// This is the first rival in Yellow. His Pokemon is used to determine the non-player
						// starter, so we can't change it here. Just skip it.
						continue;
					}
					String group = t.tag == null ? "" : t.tag;
					if (group.contains("-")) {
						group = group.substring(0, group.indexOf('-'));
					}
					if (group.startsWith("GYM") || group.startsWith("ELITE") ||
							((group.startsWith("CHAMPION") || group.startsWith("THEMED")) && !isTypeThemedEliteFourGymOnly)) {
						// Yep this is a group
						if (!groups.containsKey(group)) {
							groups.put(group, new ArrayList<>());
						}
						groups.get(group).add(t);
					} else if (group.startsWith("GIO")) {
						// Giovanni has same grouping as his gym, gym 8
						if (!groups.containsKey("GYM8")) {
							groups.put("GYM8", new ArrayList<>());
						}
						groups.get("GYM8").add(t);
					}
				}
	 */
	/*
	// Give a type to each group
				// Gym & elite types have to be unique
				// So do uber types, including the type we pick for champion
				Set<Type> usedGymTypes = new TreeSet<>();
				Set<Type> usedEliteTypes = new TreeSet<>();
				for (String group : groups.keySet()) {
					List<Trainer> trainersInGroup = groups.get(group);
					// Shuffle ordering within group to promote randomness
					Collections.shuffle(trainersInGroup, random);
					Type typeForGroup = pickType(weightByFrequency, noLegendaries, includeFormes);
					if (group.startsWith("GYM")) {
						while (usedGymTypes.contains(typeForGroup)) {
							typeForGroup = pickType(weightByFrequency, noLegendaries, includeFormes);
						}
						usedGymTypes.add(typeForGroup);
					}
					if (group.startsWith("ELITE")) {
						while (usedEliteTypes.contains(typeForGroup)) {
							typeForGroup = pickType(weightByFrequency, noLegendaries, includeFormes);
						}
						usedEliteTypes.add(typeForGroup);
					}
					if (group.equals("CHAMPION")) {
						usedUberTypes.add(typeForGroup);
					}
	
					for (Trainer t : trainersInGroup) {
						trainerTypes.put(t, typeForGroup);
					}
				}
	 */
	// List<Integer> eliteFourIndices = getEliteFourTrainers(forceChallengeMode);
	// List<Integer> mainPlaythroughTrainers = getMainPlaythroughTrainers();
}
