package vf.model

import com.dabomstew.pkrandom.pokemon.{Evolution, EvolutionType, Pokemon}
import utopia.flow.collection.immutable.Pair

object EvoState
{
	def from(evo: Evolution) = apply(Pair(evo.from, evo.to), evo.`type`, evo.extraInfo)
}

/**
 * Immutable object for representing an evo between two poke forms
 * @author Mikko Hilpinen
 * @since 26.11.2023, v1.0-alt
 */
case class EvoState(pokemons: Pair[Pokemon], evoType: EvolutionType, extraInfo: Int)
	extends EvoLike
