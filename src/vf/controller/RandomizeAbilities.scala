package vf.controller

import com.dabomstew.pkrandom.constants.{Abilities, GlobalConstants, Species}
import com.dabomstew.pkrandom.romhandlers.RomHandler
import utopia.flow.collection.CollectionExtensions._
import vf.model.EvolveGroup
import vf.poke.core.model.enumeration.PokeType
import vf.util.RandomUtils

import scala.collection.mutable
import scala.jdk.CollectionConverters._

/**
 * Randomizes poke-abilities
 * @author Mikko Hilpinen
 * @since 26.11.2023, v1.0-alt
 */
object RandomizeAbilities
{
	// private val nonTypeWeight = 0.1
	// private val typeWeightMod = 10.0
	
	private val weakAbilities = GlobalConstants.badAbilities.asScala.toSet.map { a: Integer => a: Int }
	private val negativeAbilities = GlobalConstants.negativeAbilities.asScala.toSet.map { a: Integer => a: Int }
	
	private val normalWeight = 0.3
	private val hiddenWeight = 1.0
	private val legendaryWeight = 1.2
	private val megaWeight = 2.2
	private val nonContextWeight = 0.25
	
	private val weakWeight = 0.3
	private val negativeWeight = 0.6
	
	private val notRandomizedPokeNumbers = Set(
		Species.castform, Species.darmanitan, Species.aegislash, Species.wishiwashi, Species.cherrim, Species.shedinja,
		Species.arceus, Species.mimikyu
	)
	
	// NB: For logging later
	// romHandler.abilityName(pkmn.ability1)
	
	// Intended to be called after types have been randomized
	def all(groups: Iterable[EvolveGroup])(implicit rom: RomHandler) = {
		// Groups the abilities by the number of times they appear for each original poke type
		// Also checks how many times an ability appears as a hidden ability and how often it appears on a legendary poke
		val typeAbilityCounts = PokeType.values.map { t => t -> mutable.Map[Int, Double]() }.toMap
		// Keys are abilities. Values contain 3 elements:
		// Appearances as normal abilities, appearances as hidden abilities, appearances on legendary pokes
		// and appearances as mega abilities
		val abilityContextCounts = mutable.Map[Int, (Double, Double, Double, Double)]()
		
		groups.foreach { group =>
			// Because group pokes may repeat info, decreases the impact
			val mod = 1.0 / group.size
			group.iterator.foreach { poke =>
				// There are certain form-related abilities that shouldn't get randomized
				if (!notRandomizedPokeNumbers.contains(poke.number)) {
					val types = poke.originalState.types
					val abilities = poke.originalState.abilities
						// Applies ROM-specific restrictions
						.take(rom.abilitiesPerPokemon()).filter { _._1 <= rom.highestAbilityIndex() }
					abilities.foreach { case (ability, isHidden) =>
						val primaryMap = typeAbilityCounts(types.primary)
						// Adds more emphasis on the primary type for dual types
						types.secondary match {
							case Some(secondaryType) =>
								primaryMap(ability) = primaryMap.getOrElse(ability, 0.0) + mod * 0.7
								val secondaryMap = typeAbilityCounts(secondaryType)
								secondaryMap(ability) = secondaryMap.getOrElse(ability, 0.0) + mod * 0.3
							case None => primaryMap(ability) = primaryMap.getOrElse(ability, 0.0) + mod
						}
						// Records ability context
						abilityContextCounts.updateWith(ability) { prev =>
							val (normal, hidden, legendary, mega) = prev.getOrElse((0.0, 0.0, 0.0, 0.0))
							val newStatus = {
								if (poke.isMega)
									(normal, hidden, legendary, mega + mod)
								else if (poke.isLegendary)
									(normal, hidden, legendary + mod, mega)
								else if (isHidden || abilities.size == 1)
									(normal, hidden + mod, legendary, mega)
								else
									(normal + mod, hidden, legendary, mega)
							}
							Some(newStatus)
						}
					}
				}
			}
		}
		
		// Adds one instance of wonder guard to every type (for fun)
		typeAbilityCounts.valuesIterator.foreach { t => t.updateWith(Abilities.wonderGuard) {
			case Some(count) => Some(count + 1.0)
			case None => Some(1.0)
		} }
		
		// Calculates a basic weight modifier for each ability within a type
		val typeAbilityWeights = typeAbilityCounts.view.mapValues { abilityMap =>
			val typeTotal = abilityMap.valuesIterator.sum
			abilityMap.view.mapValues { _ / typeTotal }.toMap
		}
		// Also calculates context-based weight modifiers
		val baseAbilityWeight = weightMapFor(abilityContextCounts, level = 0)
		val legendaryAbilityWeight = weightMapFor(abilityContextCounts, level = 1)
		val megaAbilityWeight = weightMapFor(abilityContextCounts, level = 2)
		
		// Now, maps the abilities of each evo-group
		groups.foreach { group =>
			// Will never swap wonder guard
			val typeWts = group.types.iterator.map { t => typeAbilityWeights(t) }
				.reduce { _.mergeWith(_) { _ + _ } }
			// Slightly WET WET here...
			lazy val baseWeights = typeWts.map { case (ability, wt) => ability -> wt * baseAbilityWeight(ability) }
			lazy val legendWeights = typeWts.map { case (ability, wt) => ability -> wt * legendaryAbilityWeight(ability) }
			lazy val megaWeights = typeWts.map { case (ability, wt) => ability -> wt * megaAbilityWeight(ability) }
			val originalAbilities = group.iterator
				.flatMap { p =>
					val level = if (p.isMega) megaWeights else if (p.isLegendary) legendWeights else baseWeights
					p.abilities.map { _._1 -> level }
				}
				.toSet.filterNot { _._1 == Abilities.wonderGuard }
			lazy val mappings = originalAbilities
				.map { case (ability, weights) => ability -> RandomUtils.weighedRandom(weights) }.toMap
			
			// Applies the mappings, except for certain pokes
			group.iterator.foreach { p => if (!notRandomizedPokeNumbers.contains(p.number)) p.mapAbilities(mappings) }
		}
	}
	
	// Level is 0 for normal, 1 for legendary and 2 for mega
	private def weightMapFor(abilityContextCounts: mutable.Map[Int, (Double, Double, Double, Double)], level: Int) = {
		abilityContextCounts.view.map { case (ability, (normal, hidden, legendary, mega)) =>
			val normalWt = if (level == 0) normalWeight else normalWeight * nonContextWeight
			val hiddenWt = if (level == 0) hiddenWeight else hiddenWeight * nonContextWeight
			val legendWt = if (level <= 1) legendaryWeight else legendaryWeight * nonContextWeight
			val base = (normal * normalWt + hidden * hiddenWt + legendary * legendWt + mega * megaWeight) /
				(normal + hidden + legendary + mega)
			val mod = {
				if (weakAbilities.contains(ability))
					weakWeight
				else if (negativeAbilities.contains(ability))
					negativeWeight
				else
					1.0
			}
			ability -> (base * mod)
		}.toMap
	}
}
