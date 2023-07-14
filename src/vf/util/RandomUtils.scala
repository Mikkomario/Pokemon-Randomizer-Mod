package vf.util

import com.dabomstew.pkrandom.RandomSource
import utopia.flow.collection.CollectionExtensions._

/**
 * Utility functions for randomization
 * @author Mikko Hilpinen
 * @since 6.7.2023, v1.0-alt
 */
object RandomUtils
{
	def chance(trueRatio: Double) = RandomSource.nextDouble() < trueRatio
	def chanceNotTo(falseRatio: Double) = chance(1 - falseRatio)
	
	def decreasedChance(trueRatio: Double, diff: Double = 0.5) = chance(trueRatio * (1 - diff))
	def increasedChance(trueRatio: Double, diff: Double = 0.5) =
		chanceNotTo((1 - trueRatio) * (1 - diff))
	
	// Generates n integers which are all different
	def distinctNextInts(count: Int, bound: Int): Set[Int] = {
		// Case: Nothing to generate
		if (count <= 0)
			Set()
		// Case: No randomization applicable
		else if (count >= bound - 1)
			(0 until bound).toSet
		// Case: Only one random number required
		else if (count == 1)
			Set(RandomSource.nextInt(bound))
		// Case: May be randomized
		else
			Iterator.continually { RandomSource.nextInt(bound) }.distinct.take(count).toSet
	}
	
	def weighedRandom[A](options: Iterable[(A, Double)]) = {
		val totalWeight = options.iterator.map { _._2 }.sum
		// Relativizes the items so that total weight is 1
		val relativized = options.map { case (item, weight) => item -> (weight / totalWeight) }
		// Finds an item at a random (relativized) spot between 0 and 1
		val threshold = RandomSource.nextDouble()
		relativized.tail.foldLeftIterator(relativized.head) { case ((_, progress), (next, advance)) =>
			next -> (progress + advance)
		}.find { _._2 >= threshold }.getOrElse(relativized.last)._1
	}
	
	def randomFrom[A](coll: Seq[A]) = coll(RandomSource.nextInt(coll.size))
}
