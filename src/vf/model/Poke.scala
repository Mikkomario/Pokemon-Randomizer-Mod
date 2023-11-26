package vf.model

import com.dabomstew.pkrandom.pokemon.{MegaEvolution, Pokemon}
import com.dabomstew.pkrandom.romhandlers.RomHandler
import utopia.flow.collection.immutable.Pair
import utopia.flow.view.mutable.caching.ResettableLazy
import vf.model.TypeSet.PokeType

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
		Pair(forms.flatMap { _.evolutionsFrom.asScala }, forms.flatMap { _.evolutionsTo.asScala })
			.map { _.distinct.map { new Evo(_) } }
	
	/**
	 * The state of this Poke as first encountered (should be the Poke's original, unmodified state)
	 */
	val originalState = PokeState.from(wrapped)
	
	private val stateCache = ResettableLazy { PokeState.from(wrapped) }
	
	
	// COMPUTED -----------------------
	
	/**
	 * @return Pokedex number of this poke.
	 *         May be game-specific.
	 */
	def number = wrapped.number
	def name = wrapped.name
	
	/**
	 * @return The current state of this poke's data
	 */
	def state = stateCache.value
	
	def primaryType_=(newType: PokeType) = {
		wrapped.primaryType = newType
		updateState()
	}
	def secondaryType_=(newType: PokeType) = {
		wrapped.secondaryType = newType
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
	/**
	 * @return Type added as the secondary type during this process
	 */
	def addedType = if (originalState.types.isSingleType) secondaryType else None
	
	def bstChange = change.mapAndMerge { _.bst } { (original, current) => current / original.toDouble }
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
	override def megaEvos: Pair[Vector[MegaEvolution]] = state.megaEvos
	override def stats: Map[PokeStat, Int] = state.stats
	override def moves: Vector[MoveLearn] = state.moves
	
	
	// OTHER    -----------------------
	
	def represents(poke: Pokemon) = wrapped == poke || cosmeticForms.contains(poke)
	
	def update(stat: PokeStat, value: Int) = {
		stat.to(wrapped, value)
		updateState()
	}
	// Swaps two stats with each other
	def swap(stats: Pair[PokeStat]): Unit = {
		val values = stats.map(apply)
		stats.mergeWith(values.reverse)(update)
	}
	def mapStat(stat: PokeStat)(f: Int => Int) = update(stat, f(apply(stat)))
	
	// Makes the specified item appear as a held item (as commonly as possible)
	def giveItem(item: Int) = {
		if (wrapped.guaranteedHeldItem >= 0)
			wrapped.guaranteedHeldItem = item
		else if (wrapped.commonHeldItem >= 0)
			wrapped.commonHeldItem = item
		else if (wrapped.rareHeldItem >= 0)
			wrapped.rareHeldItem = item
	}
	
	def updateState() = stateCache.reset()
}
