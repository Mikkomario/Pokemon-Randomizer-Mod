package vf.model

import com.dabomstew.pkrandom.constants.{Abilities, GlobalConstants}
import com.dabomstew.pkrandom.pokemon.MegaEvolution
import utopia.flow.collection.immutable.Pair
import vf.model.PokeLike.{badAbilities, negativeAbilities, wg}
import vf.model.PokeStat.{Attack, SpecialAttack}

import scala.jdk.CollectionConverters._

object PokeLike
{
	private val negativeAbilities = GlobalConstants.negativeAbilities.iterator().asScala.toSet.map { a: Integer => a: Int }
	private val badAbilities = GlobalConstants.badAbilities.iterator().asScala.toSet.map { a: Integer => a: Int }
	private val wg = Abilities.wonderGuard
}

/**
 * Common trait for models that represent a poke (state)
 * @author Mikko Hilpinen
 * @since 26.11.2023, v1.0-alt
 */
trait PokeLike[+E <: EvoLike] extends EvoAccess[E]
{
	// ABSTRACT ------------------------
	
	def types: TypeSet
	/**
	 * @return All (0-3) abilities of this poke. Abilities are represented with integers.
	 */
	def abilities: Vector[Int]
	def stats: Map[PokeStat, Int]
	def megaEvos: Pair[Vector[MegaEvolution]]
	def moves: Vector[MoveLearn]
	
	
	// COMPUTED -----------------------
	
	def primaryType = types.primary
	def secondaryType = types.secondary
	
	def abilityPowerMod = {
		val abil = abilities
		if (abil.nonEmpty) {
			val powerSum = abilities.map { ability =>
				if (negativeAbilities.contains(ability))
					0.65
				else if (badAbilities.contains(ability))
					0.9
				else if (ability == wg)
					2.0
				else
					1.0
			}.sum
			powerSum / abil.size
		}
		else
			1.0
	}
	
	/**
	 * @return The base stat total of this poke. Includes an ability-specific modifier.
	 */
	def bst = stats.valuesIterator.sum * abilityPowerMod
	def attackSpAttackRatio = apply(Attack) / apply(SpecialAttack).toDouble
	
	def fromMegaEvos = megaEvos.first
	def toMegaEvos = megaEvos.second
	def isMega = fromMegaEvos.nonEmpty
	def nonMega = !isMega
	def megaForms(implicit pokes: Pokes) = toMegaEvos.map { evo => pokes(evo.to) }
	def canMegaEvolveInTrainerBattle = toMegaEvos.exists { _.method == 1 }
	
	def evoMoves = moves.takeWhile { _.level == 0 }.map { _.move }
	def startingMoves =
		moves.view.dropWhile { _.level == 0 }.takeWhile { _.level == 1 }.map { _.move }.toVector
	
	def normalMoves = moves.dropWhile { _.level <= 1 }
	
	
	// IMPLEMENTED  ------------------
	
	override def isBasicForm = super.isBasicForm && nonMega
	
	
	// OTHER    ----------------------
	
	def apply(stat: PokeStat) = stats(stat)
}
