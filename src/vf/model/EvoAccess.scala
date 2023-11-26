package vf.model

import utopia.flow.collection.immutable.Pair

/**
 * Common trait for models that offer access to poke evos
 * @author Mikko Hilpinen
 * @since 25.11.2023, v0.1
 */
trait EvoAccess[+E <: EvoLike]
{
	// COMPUTED -----------------------
	
	/**
	 * @return The evos concerning this poke.
	 *         First the evos that lead to this poke, then the evos that start from this poke.
	 */
	def evos: Pair[Vector[E]]
	
	/**
	 * @return The evolutions that lead to this poke form
	 */
	def fromEvos = evos.first
	/**
	 * @return The evolutions that are possible from this poke form
	 */
	def toEvos = evos.second
	
	/**
	 * @return Whether this is the starting form in an evo-chain
	 */
	def isBasicForm = fromEvos.isEmpty
	def isEvolvedForm = !isBasicForm
	
	
	// OTHER    ----------------------
	
	/**
	 * @param pokeNumber Number of a poke
	 * @return Evolve that leads from the specified poke form to this form.
	 *         None if there is no such evolution.
	 */
	def evoFrom(pokeNumber: Int) = fromEvos.find { _.fromNumber == pokeNumber }
	/**
	 * @param poke A poke
	 * @return Evolve that leads from the specified poke form to this form.
	 *         None if there is no such evolution.
	 */
	def evoFrom(poke: Poke): Option[E] = fromEvos.find { _.isFrom(poke) }
	
	/**
	 * @param pokeNumber Number of a poke
	 * @return Evolve that leads from this poke to the specified poke form.
	 *         None if there is no such evolution.
	 */
	def evoTo(pokeNumber: Int) = toEvos.find { _.toNumber == pokeNumber }
	/**
	 * @param poke A poke
	 * @return Evolve that leads from this poke to the specified poke form.
	 *         None if there is no such evolution.
	 */
	def evoTo(poke: Poke): Option[E] = toEvos.find { _.isTo(poke) }
}