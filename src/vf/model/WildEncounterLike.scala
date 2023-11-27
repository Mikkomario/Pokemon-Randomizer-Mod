package vf.model

import utopia.flow.collection.immutable.range.NumericSpan

/**
 * Common trait for wild encounter representations
 * @author Mikko Hilpinen
 * @since 26.11.2023, v1.0-alt
 */
trait WildEncounterLike
{
	// ABSTRACT ---------------------
	
	def poke: Poke
	def levelRange: NumericSpan[Int]
	
	
	// COMPUTED --------------------
	
	def minLevel = levelRange.start
	def maxLevel = levelRange.end
}
