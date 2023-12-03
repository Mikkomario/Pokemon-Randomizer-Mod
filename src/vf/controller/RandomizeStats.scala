package vf.controller

import com.dabomstew.pkrandom.RandomSource
import com.dabomstew.pkrandom.constants.Species
import utopia.flow.collection.CollectionExtensions._
import utopia.flow.collection.immutable.Pair
import utopia.flow.operator.sign.Sign.{Negative, Positive}
import utopia.paradigm.transform.Adjustment
import vf.model.EvolveGroup
import vf.poke.core.model.enumeration.Stat
import vf.poke.core.model.enumeration.Stat._

import java.io.PrintWriter

/**
 * Implementation for stats-randomization
 * @author Mikko Hilpinen
 * @since 7.7.2023, v1.0-alt
 */
object RandomizeStats
{
	// ATTRIBUTES   ---------------------------
	
	private val randomizeStatChainChance = 0.7
	private val randomizeMoreChainChance = 0.7
	private val randomizeMoreDecrease = 0.1
	private val adjustment = Adjustment(0.1)
	private val maxImpact = 5
	
	private val shuffleStatChainChance = 0.5
	
	
	// OTHER    ------------------------------
	
	// Returns modifications made
	def all(groups: Iterable[EvolveGroup]) = {
		Log("stats") { writer =>
			val stateBeforeMod = groups.flatMap { g => g.iterator.map { p => p -> p.state } }.toMap
			val (modifiers, swaps) = groups.splitFlatMap { apply(_, writer) }
			// Logs the results, where changed
			writer.println("\n\n-----------")
			groups.foreach { group =>
				val poke = group.finalForm
				if (poke.originalState.stats != poke.stats) {
					val before = stateBeforeMod(poke)
					writer.println(s"${poke.name}: ${ poke.originalState.bst } => ${ before.bst } => ${ poke.bst } BST")
					Stat.values.foreach { stat =>
						writer.println(s"\t- $stat: ${ poke.originalState(stat) } => ${ before(stat) } => ${ poke(stat) }")
					}
				}
			}
			modifiers.toMap -> swaps.toMap
		}
	}
	
	// Uniformly randomizes for the whole evolve-group
	def apply(group: EvolveGroup, writer: PrintWriter) = {
		writer.println(s"\nProcessing $group\t----------------")
		// Modifies n stats by n%
		// The first modifications, up to the group's "favouriteness" level are guaranteed to be positive
		// After these, there will be a guaranteed negative mod as well
		// Legendaries get one less guaranteed positive rolls
		val guaranteedPositiveRollsCount = group.favouriteLevel + (if (group.forms.exists { _.isLegendary }) 0 else 1)
		var modsApplied = 0
		val modifiers = Iterator
			.continually {
				// Randomizes a random amount (using chaining)
				var impact = 1
				while (RandomSource.nextDouble() < randomizeMoreChainChance - randomizeMoreDecrease * (impact - 1) &&
					impact < maxImpact)
				{
					impact += 1
				}
				val direction = {
					if (modsApplied < guaranteedPositiveRollsCount)
						Positive
					else if (modsApplied == guaranteedPositiveRollsCount)
						Negative
					else if (RandomSource.nextBoolean())
						Positive
					else
						Negative
				}
				modsApplied += 1
				val mod = adjustment(direction * impact)
				// Randomizes a random stat
				Stat.values.random -> mod
			}
			// Randomizes a random number of stats
			.takeWhile { _ => RandomSource.nextDouble() < randomizeStatChainChance }
			.toVector
			// If a stat was randomized multiple times, combines the effects
			.groupMapReduce { _._1 } { _._2 } { _ * _ }
			.withDefaultValue(1.0)
		if (modifiers.nonEmpty) {
			writer.println(s"Modifies ${modifiers.size} stats:")
			modifiers.foreach { case (stat, mod) => writer.println(s"\t- $stat = ${(mod * 100).round}%") }
		}
		// Swaps stats around randomly (chaining)
		val statAssignments = {
			val defaultSwaps = Iterator
				.continually {
					val from = Stat.values.random
					val to = from.otherRandom
					Pair(from, to)
				}
				.takeWhile { _ => RandomSource.nextDouble() < shuffleStatChainChance }
				// Forms a map of the assignments
				.foldLeft(Stat.values.map { s => s -> s }.toMap) { (stats, swap) =>
					val actualSwaps = Pair(swap, swap.reverse).map { swap => swap.first -> stats(swap.second) }.toMap
					stats ++ actualSwaps
				}
				// Removes cases where the stat won't change
				.filterNot { case (to, from) => to == from }
			
			// However, if the targeted poke is Shedinja (1 HP), won't allow swaps that include HP
			// (Affects the whole group)
			if (group.forms.exists { _.number == Species.shedinja })
				defaultSwaps.filterNot { case (to, from) => Pair(to, from).contains(Hp) }
			else
				defaultSwaps
		}
		if (statAssignments.nonEmpty) {
			writer.println(s"Performs ${statAssignments.size} stat swaps")
			statAssignments.foreach { case (from, to) => writer.println(s"\t- $from => $to") }
		}
		
		// Applies the shuffles and the modifiers to all pokemon in this group
		// Saves the original BST (base-stat-total) values
		group.iterator.foreach { poke =>
			statAssignments.foreach { swap => poke.swap(Pair.tupleToPair(swap)) }
			modifiers.foreachEntry { (stat, mod) =>
				poke.mapStat(stat) { v => ((v * mod).toInt max stat.minimumValue) min Stat.maxValue }
			}
		}
		
		// Returns the applied modifications
		val modifiersToReturn = {
			if (modifiers.isEmpty)
				Map[Int, Map[Stat, Double]]()
			else
				group.iterator.map { _.number -> modifiers }.toMap
		}
		val assignmentsToReturn = {
			if (statAssignments.isEmpty)
				Map[Int, Map[Stat, Stat]]()
			else
				group.iterator.map { _.number -> statAssignments }.toMap
		}
		modifiersToReturn -> assignmentsToReturn
	}
}
