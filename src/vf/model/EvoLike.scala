package vf.model

import com.dabomstew.pkrandom.pokemon.{EvolutionType, Pokemon}
import utopia.flow.collection.immutable.Pair
import vf.model.EvoLike.{itemTypes, stoneTypes}

object EvoLike
{
	val stoneTypes = Set(EvolutionType.STONE, EvolutionType.STONE_MALE_ONLY, EvolutionType.STONE_FEMALE_ONLY)
	val itemTypes = Set(EvolutionType.LEVEL_ITEM_DAY, EvolutionType.LEVEL_ITEM_NIGHT) ++ stoneTypes
}

/**
 * An immutable model that represents a change from one poke form to another under certain specific conditions
 * @author Mikko Hilpinen
 * @since 25.11.2023, v1.0-alt
 */
trait EvoLike
{
	// ABSTRACT ----------------------
	
	def pokemons: Pair[Pokemon]
	def evoType: EvolutionType
	def extraInfo: Int
	
	
	// COMPUTED ----------------------
	
	/**
	 * @return Whether this evo is level-based
	 */
	def usesLevel: Boolean = evoType.usesLevel()
	def nonLevelBased = !usesLevel
	def levelThreshold: Option[Int] = if (usesLevel) Some(extraInfo) else None
	
	def involvesItem = itemTypes.contains(evoType)
	def item = if (involvesItem) Some(extraInfo) else None
	
	// Stones & other items are represented with integers
	def stone: Option[Int] = if (usesStone) Some(extraInfo) else None
	
	def usesStone = stoneTypes.contains(evoType)
	def nonStoneBased = !usesStone
	
	def fromNumber = pokemons.first.number
	def toNumber = pokemons.second.number
	def numbers = pokemons.map { _.number }
	
	def from(implicit pokes: Pokes) = pokes(pokemons.first)
	def to(implicit pokes: Pokes) = pokes(pokemons.second)
	def ends(implicit pokes: Pokes) = pokemons.map { pokes(_) }
	
	def isTo(poke: Poke) = poke.represents(pokemons.second)
	def isFrom(poke: Poke) = poke.represents(pokemons.first)
	
	
	// OTHER    ----------------------
	
	def usesStone(stone: Int): Boolean = usesStone && this.stone.contains(stone)
}