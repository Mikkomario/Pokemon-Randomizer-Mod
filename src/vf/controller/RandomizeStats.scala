package vf.controller

import com.dabomstew.pkrandom.RandomSource
import vf.model.{EvolveGroup, PokeStat}
import utopia.flow.collection.CollectionExtensions._
import utopia.flow.collection.immutable.Pair
import vf.util.PokemonExtensions._

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
	
	def all()(implicit groups: IterableOnce[EvolveGroup]): Unit = groups.iterator.foreach(apply)
	
	// Uniformly randomizes for the whole evolve-group
	def apply(group: EvolveGroup) = {
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
		val shuffles = Iterator
			.continually {
				val from = PokeStat.random
				val to = from.otherRandom
				Pair(from, to)
			}
			.takeWhile { _ => RandomSource.nextDouble() < shuffleStatChainChance }
			.toVector
		// Applies the shuffles and the modifiers to all pokemon in this group
		group.iterator.foreach { poke =>
			shuffles.foreach(poke.swap)
			modifiers.foreachEntry { (stat, mod) =>
				poke.mapStat(stat) { v => ((v * mod).toInt max stat.minimumValue) min PokeStat.maxValue }
			}
		}
	}
}
