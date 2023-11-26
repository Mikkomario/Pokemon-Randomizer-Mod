package vf.model

import com.dabomstew.pkrandom.pokemon.MegaEvolution
import utopia.flow.collection.immutable.Pair
import vf.model.PokeStat.{Attack, SpecialAttack}

/**
 * Common trait for models that represent a poke (state)
 * @author Mikko Hilpinen
 * @since 26.11.2023, v1.0-alt
 */
trait PokeLike[+E <: EvoLike] extends EvoAccess[E]
{
	// ABSTRACT ------------------------
	
	def types: TypeSet
	def stats: Map[PokeStat, Int]
	def megaEvos: Pair[Vector[MegaEvolution]]
	def moves: Vector[MoveLearn]
	
	
	// COMPUTED -----------------------
	
	def primaryType = types.primary
	def secondaryType = types.secondary
	
	// BST = base stat total
	def bst = stats.valuesIterator.sum
	def attackSpAttackRatio = apply(Attack) / apply(SpecialAttack).toDouble
	
	def megaForms(implicit pokes: Pokes) = megaEvos.second.map { evo => pokes(evo.to) }
	
	def evoMoves = moves.takeWhile { _.level == 0 }.map { _.move }
	def startingMoves =
		moves.view.dropWhile { _.level == 0 }.takeWhile { _.level == 1 }.map { _.move }.toVector
	
	def normalMoves = moves.dropWhile { _.level <= 1 }
	
	
	// OTHER    ----------------------
	
	def apply(stat: PokeStat) = stats(stat)
}
