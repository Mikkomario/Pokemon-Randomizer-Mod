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
