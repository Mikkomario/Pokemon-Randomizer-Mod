package vf.model

import com.dabomstew.pkrandom.pokemon.Encounter
import utopia.flow.collection.immutable.range.NumericSpan
import utopia.flow.view.mutable.caching.ResettableLazy

import scala.util.Random

/**
 * Wrapper class for the java-based encounter class
 * @author Mikko Hilpinen
 * @since 26.11.2023, v1.0-alt
 */
class WildEncounter(val zone: Int, val wrapped: Encounter)(implicit pokes: Pokes) extends WildEncounterLike
{
	// ATTRIBUTES   -----------------------
	
	val originalState = WildEncounterState.from(wrapped)
	
	private val stateCache = ResettableLazy { WildEncounterState.from(wrapped) }
	
	
	// COMPUTED ---------------------------
	
	def state = stateCache.value
	
	def poke_=(newPoke: Poke) = {
		// TODO: Possibly assign a random form here
		wrapped.pokemon = newPoke.wrapped
		updateState()
	}
	
	
	// IMPLEMENTED  -----------------------
	
	override def poke: Poke = state.poke
	override def levelRange: NumericSpan[Int] = state.levelRange
	
	
	// OTHER    ---------------------------
	
	def scaleLevelBy(mod: Double) = {
		if (mod < 1.0) {
			wrapped.level = (wrapped.level * mod).toInt max 1
			wrapped.maxLevel = (wrapped.maxLevel * mod).toInt max wrapped.level
			updateState()
		}
		else if (mod > 1.0) {
			wrapped.level = (wrapped.level * mod).ceil.toInt min 100
			wrapped.maxLevel = (wrapped.maxLevel * mod).ceil.toInt min 100
			updateState()
		}
	}
	
	def setForme() = {
		val form = poke.randomForm
		if (form.formeNumber > 0) {
			wrapped.formeNumber = form.formeNumber
			wrapped.pokemon = form.baseForme
			if (form.cosmeticForms > 0)
				wrapped.formeNumber += form.getCosmeticFormNumber(Random.nextInt(form.cosmeticForms))
		}
		else if (form.cosmeticForms > 0)
			wrapped.formeNumber = form.getCosmeticFormNumber(Random.nextInt(form.cosmeticForms))
		else
			wrapped.formeNumber = 0
	}
	
	def updateState() = stateCache.reset()
}
