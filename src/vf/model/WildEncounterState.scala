package vf.model

import com.dabomstew.pkrandom.pokemon.Encounter
import utopia.flow.collection.immutable.range.NumericSpan

object WildEncounterState
{
	def from(enc: Encounter)(implicit pokes: Pokes) =
		apply(pokes(enc.pokemon), NumericSpan(enc.level, enc.maxLevel))
}

/**
 * An immutable model that represents a wild poke encounter
 * @author Mikko Hilpinen
 * @since 26.11.2023, v1.0-alt
 */
case class WildEncounterState(poke: Poke, levelRange: NumericSpan[Int]) extends WildEncounterLike
