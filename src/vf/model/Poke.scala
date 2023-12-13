package vf.model

import com.dabomstew.pkrandom.pokemon.{MegaEvolution, Pokemon}
import com.dabomstew.pkrandom.romhandlers.RomHandler
import utopia.flow.collection.CollectionExtensions._
import utopia.flow.collection.immutable.Pair
import utopia.flow.view.mutable.caching.ResettableLazy
import vf.controller.Settings
import vf.poke.core.model.cached.TypeSet
import vf.poke.core.model.enumeration.{PokeType, Stat}
import vf.util.PokeExtensions._

import scala.jdk.CollectionConverters._
import scala.language.implicitConversions

/**
 * A mutable interface for interacting with Poke-related data
 * @author Mikko Hilpinen
 * @since 25.11.2023, v0.1
 */
class Poke(val wrapped: Pokemon, val cosmeticForms: Vector[Pokemon] = Vector())(implicit rom: RomHandler)
	extends PokeLike[Evo]
{
	// ATTRIBUTES   -------------------
	
	override lazy val evos: Pair[Vector[Evo]] =
		Pair(forms.flatMap { _.evolutionsTo.asScala }, forms.flatMap { _.evolutionsFrom.asScala })
			.map { _.distinct.map { new Evo(_) } }
	
	/**
	 * The state of this Poke as first encountered (should be the Poke's original, unmodified state)
	 */
	val originalState = PokeState.from(wrapped)
	
	/**
	 * Whether this is an "eviolite" poke, which shouldn't evolve
	 */
	lazy val isEviolitePoke = Settings.isEviolitePoke(this)
	
	private val stateCache = ResettableLazy { PokeState.from(wrapped) }
	
	
	// COMPUTED -----------------------
	
	/**
	 * @return Pokedex number of this poke.
	 *         May be game-specific.
	 */
	def number = wrapped.number
	def name = wrapped.fullName
	
	def isLegendary = wrapped.isLegendary
	def nonLegendary = !isLegendary
	
	def randomForm = forms.random
	
	/**
	 * @return 1-based indices of the valid ability slots for this poke
	 */
	def abilitySlots = Vector(wrapped.ability1, wrapped.ability2, wrapped.ability3)
		.view.zipWithIndex.filter { _._1 != 0 }.map { _._2 + 1 }.toVector
	
	/**
	 * @return The current state of this poke's data
	 */
	def state = stateCache.value
	
	def primaryType_=(newType: PokeType) = {
		forms.foreach { _.primaryType = newType }
		updateState()
	}
	def secondaryType_=(newType: PokeType) = {
		forms.foreach { _.secondaryType = newType }
		updateState()
	}
	
	def change = Pair(originalState, state)
	
	def typeSwaps = {
		val c = change
		val primaryChange = Some(c.map { _.primaryType }).filter { _.isAsymmetric }
		val secondaryChange = Some(c.map { _.secondaryType })
			.filter { p => p.forall { _.isDefined } && p.isAsymmetric }.map { _.map { _.get } }
		Vector(primaryChange, secondaryChange).flatten.map { _.toTuple }.toMap
	}
	def preservedOriginalTypes = originalState.types.types.toSet & types.types.toSet
	/**
	 * @return Type added as the secondary type during this process
	 */
	def addedType = if (originalState.types.isSingleType) secondaryType else None
	
	def bstChange = change.mapAndMerge { _.bst } { (original, current) => current / original }
	/**
	 * @return Amount of change occurred in attack to special attack -ratio during modifications.
	 *         1.0 means no change. 2.0 means attack has relatively doubled.
	 *         0.5 means that special attack has relatively doubled.
	 */
	def attackSpAttackRatioChange = attackSpAttackRatio / originalState.attackSpAttackRatio
	def attackToSpAttackRatioHasReversed = {
		val original = originalState.attackSpAttackRatio
		val current = attackSpAttackRatio
		if (original < 0.95)
			current > 1.1
		else
			original > 1.1 && current < 0.95
	}
	
	private def forms = wrapped +: cosmeticForms
	
	
	// IMPLEMENTED  -------------------
	
	override def types: TypeSet = state.types
	override def abilities: Vector[(Int, Boolean)] = state.abilities
	override def megaEvos: Pair[Vector[MegaEvolution]] = state.megaEvos
	override def stats: Map[Stat, Int] = state.stats
	override def moves: Vector[MoveLearn] = state.moves
	
	
	// OTHER    -----------------------
	
	def represents(poke: Pokemon) = wrapped == poke || cosmeticForms.contains(poke)
	
	def mapAbilities(mappings: Map[Int, Int]) = {
		wrapped.ability1 = mappings.getOrElse(wrapped.ability1, wrapped.ability1)
		wrapped.ability2 = mappings.getOrElse(wrapped.ability2, wrapped.ability2)
		wrapped.ability3 = mappings.getOrElse(wrapped.ability3, wrapped.ability3)
		updateState()
	}
	
	def update(stat: Stat, value: Int) = {
		forms.foreach { _(stat) = value }
		updateState()
	}
	// Swaps two stats with each other
	def swap(stats: Pair[Stat]): Unit = {
		val values = stats.map(apply)
		stats.mergeWith(values.reverse)(update)
	}
	def mapStat(stat: Stat)(f: Int => Int) = update(stat, f(apply(stat)))
	def scaleStats(scalingMod: Double) = Stat.values
		.foreach { stat => mapStat(stat) { s => (s * scalingMod).round.toInt } }
	
	// Makes the specified item appear as a held item (as commonly as possible)
	def giveItem(item: Int) = {
		forms.foreach { p =>
			if (p.guaranteedHeldItem >= 0)
				p.guaranteedHeldItem = item
			else if (p.commonHeldItem >= 0)
				p.commonHeldItem = item
			else if (p.rareHeldItem >= 0)
				p.rareHeldItem = item
		}
	}
	
	def updateState() = stateCache.reset()
}
