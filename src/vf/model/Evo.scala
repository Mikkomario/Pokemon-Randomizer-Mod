package vf.model

import com.dabomstew.pkrandom.pokemon.{Evolution, EvolutionType, Pokemon}
import utopia.flow.collection.immutable.Pair
import utopia.flow.view.mutable.caching.ResettableLazy

/**
 * Provides an mutable interface to a Poke's singular evolution
 * @author Mikko Hilpinen
 * @since 25.11.2023, v1.0-alt
 */
class Evo(val wrapped: Evolution) extends EvoLike
{
	// ATTRIBUTES   ---------------------
	
	val originalState = EvoState.from(wrapped)
	
	private val stateCache = ResettableLazy { EvoState.from(wrapped) }
	
	
	// COMPUTED -------------------------
	
	def state = stateCache.value
	
	def carriesStats = wrapped.carryStats
	
	def levelThreshold_=(newThreshold: Int) = {
		if (usesLevel) {
			wrapped.extraInfo = newThreshold
			updateState()
		}
		else
			makeLevelBased(newThreshold)
	}
	
	
	// IMPLEMENTED  ---------------------
	
	override def pokemons: Pair[Pokemon] = state.pokemons
	override def evoType: EvolutionType = state.evoType
	override def extraInfo: Int = state.extraInfo
	
	override def toString = s"${wrapped.from.name} => ${wrapped.to.name}"
	
	
	// OTHER    ------------------------
	
	/**
	 * Makes this evo fully level-based
	 * @param levelThreshold Level at which this evo should occur
	 */
	def makeLevelBased(levelThreshold: Int) = {
		wrapped.`type` = EvolutionType.LEVEL
		wrapped.extraInfo = levelThreshold
		updateState()
	}
	def makeStoneBased(stone: Int) = {
		wrapped.`type` = EvolutionType.STONE
		wrapped.extraInfo = stone
		updateState()
	}
	
	private def updateState() = stateCache.reset()
}
