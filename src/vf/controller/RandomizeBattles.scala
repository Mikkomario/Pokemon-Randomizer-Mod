package vf.controller

import com.dabomstew.pkrandom.pokemon.Trainer
import com.dabomstew.pkrandom.romhandlers.RomHandler
import com.dabomstew.pkrandom.{RandomSource, Settings}
import utopia.flow.collection.immutable.range.Span
import utopia.flow.util.NotEmpty
import utopia.flow.collection.CollectionExtensions._
import utopia.flow.view.mutable.eventful.Flag
import vf.model.TypeRelation.{Relative, StrongRelative, Unrelated, WeakRelative}
import vf.model._
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
	
	def all(pokeMapping: Map[Int, Vector[EvolveGroup]], minAppearanceLevels: Map[EvolveGroup, Int])
	       (implicit rom: RomHandler, pokes: Pokes, settings: Settings) =
	{
		val levelMod = 1 + settings.getTrainersLevelModifier / 100.0
		// Indices poke groups by poke number. Only includes poke groups that appear in the 'minAppearanceLevels'
		// (i.e. the wild)
		val pokePool = minAppearanceLevels.keySet
		val groupByNumber = pokePool.flatMap { g => g.iterator.map { _.number -> g } }.toMap
		val trainers = rom.getTrainers
		trainers.iterator().asScala
			// This is the first rival in Yellow. His Pokemon is used to determine the non-player
			// starter, so we can't change it here. Just skip it.
			.filterNot { t => Option(t.tag).contains("IRIVAL") }
			.foreach { trainer => apply(trainer, levelMod, pokeMapping, pokePool, groupByNumber, minAppearanceLevels) }
		// TODO: Is double battle mode correct?
		// Saves the changes to the ROM
		rom.setTrainers(trainers, false)
	}
	
	// TODO: Current implementation is missing rival starter handling. See if that needs to be added separately.
	def apply(trainer: Trainer, levelMod: Double,
	          pokeMapping: Map[Int, Vector[EvolveGroup]], pokePool: Iterable[EvolveGroup], groups: Map[Int, EvolveGroup],
	          minAppearanceLevels: Map[EvolveGroup, Int])
	         (implicit rom: RomHandler, pokes: Pokes, settings: Settings) =
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
		// Can only apply one mega evo per trainer
		val megaEvoFlag = Flag()
		Random.shuffle(trainer.pokemon.asScala).foreach { tp =>
			val originalNumber = tp.pokemon.number
			val originalGroup = groups(originalNumber)
			val level = math.round(tp.level * levelMod).toInt
			// If the original poke can mega-evolve, requires the resulting poke to be able to do that as well
			val requireMegaEvolvable = tp.canMegaEvolve
			// Looks for a mapping result first
			// TODO: Apply stricter type-checking here also?
			val mappingOptions = pokeMapping.getOrElse(originalNumber, Vector())
				.filter { g => (!requireMegaEvolvable || g.canMegaEvolve) &&
					minAppearanceLevels.get(g).forall { _ <= level } && !selectedGroups.contains(g) }
			val (selectedGroup, selectedPoke) = {
				// Case: Mapping result available => Selects one randomly
				if (mappingOptions.nonEmpty) {
					val group = randomFrom(mappingOptions)
					group -> group.formAtLevel(level)
				}
				// Case: No mapping result available => Selects randomly from the pool
				else
					findMatchFor(pokes(tp.pokemon), originalGroup, level, pokePool, selectedGroups, minAppearanceLevels,
						useStrictTyping, requireMegaEvolvable)
			}
			selectedGroups += selectedGroup
			
			// Assigns the selected poke
			// TODO: Apply random form
			tp.pokemon = selectedPoke.randomForm
			tp.level = level
			tp.abilitySlot = selectedPoke.abilitySlots.random
			tp.resetMoves = true
			
			// Enables mega evolution for all pokes that support it
			NotEmpty(selectedPoke.toMegaEvos.filter { _.method == 1 }).foreach { megaEvos =>
				if (megaEvoFlag.set()) {
					val megaEvo = megaEvos.random
					tp.heldItem = megaEvo.argument
				}
			}
			
			// Applies shiny chance (optional)
			if (settings.isShinyChance && RandomSource.nextInt(256) == 0)
				tp.IVs |= (1 << 30)
		}
	}
	
	private def findMatchFor(original: Poke, originalGroup: EvolveGroup, level: Int, pool: Iterable[EvolveGroup],
	                         used: mutable.Set[EvolveGroup], minAppearanceLevels: Map[EvolveGroup, Int],
	                         useStrictTyping: Boolean, requireMegaEvolvable: Boolean)
	                        (implicit rom: RomHandler) =
	{
		// Applies the following filters:
		//      1) Min appearance level filter
		//      2) Non-duplicate filter
		//      3) BST filter
		//      4) Mega filter
		lazy val originalBst = original.originalState.bst
		lazy val allowedBst = bstRange.mapEnds { _ * originalBst }
		lazy val originalType = original.originalState.types
		lazy val typeRelations = TypeRelations.of(originalType)
		
		val options = pool.flatMap { group =>
			// 1, 2 & 4
			if ((!requireMegaEvolvable || group.canMegaEvolve) &&
				minAppearanceLevels.get(group).forall { _ <= level } && !used.contains(group))
			{
				val form = group.formAtLevel(level)
				val bstRatio = form.bstChange
				// 3
				if (allowedBst.contains(bstRatio)) {
					// May apply type-filtering also
					val typeRelation = typeRelations(form.types)
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
