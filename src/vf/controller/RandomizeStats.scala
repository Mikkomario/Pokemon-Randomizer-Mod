package vf.controller

import com.dabomstew.pkrandom.RandomSource
import com.dabomstew.pkrandom.constants.Species
import utopia.flow.collection.CollectionExtensions._
import utopia.flow.collection.immutable.Pair
import vf.model.PokeStat.Hp
import vf.model.{EvolveGroup, PokeStat}

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
	private val randomizeAmount = 0.1
	
	private val shuffleStatChainChance = 0.7
	
	
	// OTHER    ------------------------------
	
	// Returns modifications made
	def all()(implicit groups: IterableOnce[EvolveGroup]) = {
		val (modifiers, swaps) = groups.iterator.splitFlatMap(apply)
		modifiers.toMap -> swaps.toMap
	}
	
	// Uniformly randomizes for the whole evolve-group
	def apply(group: EvolveGroup) = {
		// Movies n stats by n%
		val modifiers = Iterator
			.continually {
				val modifierIter = {
					// Increase
					if (RandomSource.nextBoolean())
						Iterator.iterate(1 + randomizeAmount) { math.pow(_, 2) }
					// Decrease
					else
						Iterator.iterate(1 - randomizeAmount) { math.pow(_, 2) }
				}
				// Randomizes a random amount (using chaining)
				var modifier = modifierIter.next()
				while (RandomSource.nextDouble() < randomizeMoreChainChance) {
					modifier = modifierIter.next()
				}
				// Randomizes a random stat
				PokeStat.random -> modifier
			}
			// Randomizes a random number of stats
			.takeWhile { _ => RandomSource.nextDouble() < randomizeStatChainChance }
			.toVector
			// If a stat was randomized multiple times, combines the effects
			.groupMapReduce { _._1 } { _._2 } { _ * _ }
			.withDefaultValue(1.0)
		// Swaps stats around randomly (chaining)
		val statAssignments = {
			val defaultSwaps = Iterator
				.continually {
					val from = PokeStat.random
					val to = from.otherRandom
					Pair(from, to)
				}
				.takeWhile { _ => RandomSource.nextDouble() < shuffleStatChainChance }
				// Forms a map of the assignments
				.foldLeft(PokeStat.values.map { s => s -> s }.toMap) { (stats, swap) =>
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
		
		// Applies the shuffles and the modifiers to all pokemon in this group
		// Saves the original BST (base-stat-total) values
		group.iterator.foreach { poke =>
			statAssignments.foreach { swap => poke.swap(Pair.tupleToPair(swap)) }
			modifiers.foreachEntry { (stat, mod) =>
				poke.mapStat(stat) { v => ((v * mod).toInt max stat.minimumValue) min PokeStat.maxValue }
			}
		}
		
		// Returns the applied modifications
		val modifiersToReturn = {
			if (modifiers.isEmpty)
				Map[Int, Map[PokeStat, Double]]()
			else
				group.iterator.map { _.number -> modifiers }.toMap
		}
		val assignmentsToReturn = {
			if (statAssignments.isEmpty)
				Map[Int, Map[PokeStat, PokeStat]]()
			else
				group.iterator.map { _.number -> statAssignments }.toMap
		}
		modifiersToReturn -> assignmentsToReturn
	}
}
