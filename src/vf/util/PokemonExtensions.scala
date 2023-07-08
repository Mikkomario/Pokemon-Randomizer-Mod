package vf.util

import com.dabomstew.pkrandom.pokemon.Pokemon
import utopia.flow.collection.immutable.Pair
import vf.model.{PokeStat, Types}

/**
 * @author Mikko Hilpinen
 * @since 7.7.2023, v1.0-alt
 */
object PokemonExtensions
{
	implicit class RichPokemon(val p: Pokemon) extends AnyVal
	{
		def types(implicit types: Types) = types(p)
		
		def apply(stat: PokeStat) = stat.of(p)
		def update(stat: PokeStat, value: Int) = stat.to(p, value)
		// Swaps two stats with each other
		def swap(stats: Pair[PokeStat]): Unit = {
			val values = stats.map(apply)
			stats.mergeWith(values.reverse)(update)
		}
		def mapStat(stat: PokeStat)(f: Int => Int) = update(stat, f(apply(stat)))
		
		// Makes the specified item appear as a held item (as commonly as possible)
		def giveItem(item: Int) = {
			if (p.guaranteedHeldItem >= 0)
				p.guaranteedHeldItem = item
			else if (p.commonHeldItem >= 0)
				p.commonHeldItem = item
			else if (p.rareHeldItem >= 0)
				p.rareHeldItem = item
		}
	}
}
