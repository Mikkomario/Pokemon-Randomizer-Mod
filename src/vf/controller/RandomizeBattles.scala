package vf.controller

import com.dabomstew.pkrandom.{RandomSource, Settings}
import com.dabomstew.pkrandom.pokemon.{Pokemon, Trainer}
import com.dabomstew.pkrandom.romhandlers.RomHandler
import utopia.flow.collection.immutable.caching.cache.Cache
import utopia.flow.collection.immutable.range.Span
import vf.model.TypeRelation.{Relative, StrongRelative, Unrelated, WeakRelative}
import vf.model.{EvolveGroup, TypeRelation, TypeRelations, Types}
import vf.util.RandomUtils._

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
	
	private val minStrictTypeRelation: TypeRelation = Relative
	
	private val bstDiffWeightMod = 0.5
	private val typeWeights = Map[TypeRelation, Double](
		StrongRelative -> 8.0, Relative -> 6.0, WeakRelative -> 3.0, Unrelated -> 1.5
	)
	
	
	// OTHER    --------------------------
	
	def all() = ???
	
	def apply(trainer: Trainer, levelMod: Double, originalTypes: Types, currentTypes: Types,
	          originalBstValues: Map[Int, Int], currentBstValues: Map[Int, Int],
	          pokeMapping: Map[Int, Vector[EvolveGroup]], pokePool: Vector[EvolveGroup], groups: Map[Int, EvolveGroup],
	          minAppearanceLevels: Map[EvolveGroup, Int])
	         (implicit rom: RomHandler, settings: Settings) =
	{
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
		Random.shuffle(trainer.pokemon.asScala).foreach { tp =>
			val originalNumber = tp.pokemon.number
			val originalGroup = groups(originalNumber)
			val level = math.round(tp.level * levelMod).toInt
			// Looks for a mapping result first
			// TODO: Apply stricter type-checking here also?
			val mappingOptions = pokeMapping.getOrElse(originalNumber, Vector())
				.filter { g => minAppearanceLevels.get(g).forall { _ <= level } && !selectedGroups.contains(g) }
			val (selectedGroup, selectedPoke) = {
				// Case: Mapping result available => Selects one randomly
				if (mappingOptions.nonEmpty) {
					val group = randomFrom(mappingOptions)
					group -> group.formAtLevel(level)
				}
				// Case: No mapping result available => Selects randomly from the pool
				else
					findMatchFor(tp.pokemon, originalGroup, level, pokePool, selectedGroups, minAppearanceLevels,
						originalTypes, currentTypes, originalBstValues, currentBstValues, useStrictTyping)
			}
			selectedGroups += selectedGroup
			
			// Assigns the selected poke
			tp.pokemon = selectedPoke
			tp.level = level
			// tp.abilitySlot = getRandomAbilitySlot(newPK);
			tp.resetMoves = true
			
			// Applies shiny chance (optional)
			if (settings.isShinyChance && RandomSource.nextInt(256) == 0)
				tp.IVs |= (1 << 30)
		}
	}
	
	// TODO: Handle mega-evolve swap
	private def findMatchFor(original: Pokemon, originalGroup: EvolveGroup, level: Int, pool: Vector[EvolveGroup],
	                         used: mutable.Set[EvolveGroup], minAppearanceLevels: Map[EvolveGroup, Int],
	                         originalTypes: Types, currentTypes: Types,
	                         originalBstValues: Map[Int, Int], currentBstValues: Map[Int, Int], useStrictTyping: Boolean)
	                        (implicit rom: RomHandler) =
	{
		// Applies the following filters:
		//      1) Min appearance level filter
		//      2) Non-duplicate filter
		//      3) BST filter
		lazy val originalBst = originalBstValues(original.number).toDouble
		lazy val allowedBst = bstRange.mapEnds { _ * originalBst }
		lazy val originalType = originalTypes(original)
		lazy val typeRelations = TypeRelations.of(originalType)
		
		val options = pool.flatMap { group =>
			// 1 & 2
			if (minAppearanceLevels.get(group).forall { _ <= level } && !used.contains(group)) {
				val form = group.formAtLevel(level)
				val bstRatio = currentBstValues(form.number) / originalBst
				// 3
				if (allowedBst.contains(bstRatio)) {
					// May apply type-filtering also
					val typeRelation = typeRelations(currentTypes(form))
					if (!useStrictTyping || typeRelation >= minStrictTypeRelation) {
						// Calculates a weight modifier based on BST and type
						val bstWeight = math.pow(1 - (1 - bstRatio).abs, bstDiffWeightMod)
						val typeWeight = typeWeights.getOrElse(typeRelation, 1.0)
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
		if (options.isEmpty)
			originalGroup -> original
		else
			weighedRandom(options)
	}
	
	// Math.round(tp.level * (1 + levelModifier / 100.0))
	// List<Trainer> currentTrainers = this.getTrainers();
	/*
	boolean includeFormes = settings.isAllowTrainerAlternateFormes();
			boolean banIrregularAltFormes = settings.isBanIrregularAltFormes();
			boolean swapMegaEvos = settings.isSwapTrainerMegaEvos();
	 */
	// oolean shinyChance = settings.isShinyChance();
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
	/*
	if (t.tag != null && t.tag.equals("IRIVAL")) {
					// This is the first rival in Yellow. His Pokemon is used to determine the non-player
					// starter, so we can't change it here. Just skip it.
					continue;
				}
	 */
	// boolean swapThisMegaEvo = swapMegaEvos && tp.canMegaEvolve();
	
	/*
	f (swapThisMegaEvo) {
						tp.heldItem = newPK
										.megaEvolutionsFrom
										.get(this.random.nextInt(newPK.megaEvolutionsFrom.size()))
										.argument;
					}
	 */
	
	/*
	// Save it all up
			this.setTrainers(currentTrainers, false);
	 */
	/*
	if (swapMegaEvos) {
				pickFrom = megaEvolutionsList
						.stream()
						.filter(mega -> mega.method == 1)
						.map(mega -> mega.from)
						.distinct()
						.collect(Collectors.toList());
			} else {
				pickFrom = cachedAllList;
			}
	 */
}
