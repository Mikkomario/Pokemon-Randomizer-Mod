package vf.controller

import utopia.flow.collection.immutable.Pair
import utopia.flow.collection.immutable.caching.cache.Cache
import vf.model.PokeStat.{Attack, Defense, SpecialAttack, SpecialDefense}
import vf.model.{EffectivenessRelations, Poke, PokeStat, TypeSet}

/**
 * An algorithm for compensating for weaker and stronger types in the game
 * @author Mikko Hilpinen
 * @since 28.11.2023, v1.0-alt
 */
object CompensateForType
{
	private val ignoredVariance = 0.15
	private val buffPer10PercentVariance = 0.04
	private val debuffPer10PercentVariance = 0.03
	
	/**
	 * Adjusts some poke's offensive and defensive stats in order to compensate for their relatively
	 * weak or strong types
	 * @param pokePool Pokes to adjust
	 * @param encounterCounts Number of encounters for each poke (that has an encounter)
	 */
	def apply(pokePool: Iterable[Poke], encounterCounts: Map[Poke, Int]): Unit = {
		// Calculates the defensive type-based score of every poke against every enemy,
		// summing the score together to an average score
		val averageDefenseScore = apply(pokePool, encounterCounts,
			Pair(Defense, SpecialDefense)) { _.defenseRatingAgainst(_) }
		// Calculates a similar score in terms of offense
		val averageAttackScore = apply(pokePool, encounterCounts,
			Pair(Attack, SpecialAttack)) { _.offenseRatingAgainst(_) }
		
		// Logs the applied changes
		Log("type-strength-compensation") { writer =>
			writer.println("Applies the following type-specific buffs & debuffs:")
			pokePool.map { p: Poke => p.types }.toSet.foreach { types: TypeSet =>
				writer.println(s"\t- $types")
				val effectiveness = types.effectiveness
				// Calculates the defensive and offensive scores for the type
				val scores = encounterCounts.iterator
					.map { case (opponent, encounters) =>
						val offensive = effectiveness.offenseRatingAgainst(opponent.types)
						val defensive = effectiveness.defenseRatingAgainst(opponent.types)
						Pair(offensive, defensive).map { _ * encounters }
					}
					.reduce { _.mergeWith(_) { _ + _ } }
				// Calculates a score for every pokemon, and the average score
				val variances = scores.mergeWith(Pair(averageDefenseScore, averageAttackScore)) { (score, average) =>
					(score - average) / average
				}
				variances.mergeWith(Pair("Defense", "Offense")) { (variance, title) =>
					if (variance > ignoredVariance) {
						val debuff = debuffPer10PercentVariance * (variance - ignoredVariance) / 10.0
						writer.println(s"\t\t- $title: -${ (debuff * 100).round.toInt }%")
					}
					else if (variance < -ignoredVariance) {
						val buff = buffPer10PercentVariance * (variance.abs - ignoredVariance) / 10.0
						writer.println(s"\t\t- $title: +${ (buff * 100).round.toInt }%")
					}
					else
						writer.println(s"\t\t- $title: No change")
				}
			}
		}
	}
	
	private def apply(pokePool: Iterable[Poke], encounterCounts: Map[Poke, Int], affectedStats: Pair[PokeStat])
	                 (scoreFunction: (EffectivenessRelations, TypeSet) => Double) =
	{
		// Doesn't recalculate score for repeating types
		val scoreCache = Cache { types: TypeSet =>
			val effectiveness = types.effectiveness
			encounterCounts.iterator
				.map { case (opponent, encounters) => scoreFunction(effectiveness, opponent.types) * encounters }
				.sum
		}
		// Calculates a score for every pokemon, and the average score
		val scoresPerPoke = pokePool.map { poke => poke -> scoreCache(poke.types) }
		val averageScore = scoresPerPoke.iterator.map { _._2 }.sum / scoresPerPoke.size
		
		// Adjusts every pokemon according to their score
		scoresPerPoke.foreach { case (poke, score) =>
			val variance = (score - averageScore) / averageScore
			if (variance > ignoredVariance) {
				val debuff = debuffPer10PercentVariance * (variance - ignoredVariance) / 10.0
				affectedStats.foreach { poke.mapStat(_) { s => (s * (1 - debuff)).round.toInt } }
			}
			else if (variance < -ignoredVariance) {
				val buff = buffPer10PercentVariance * (variance.abs - ignoredVariance) / 10.0
				affectedStats.foreach { poke.mapStat(_) { s => (s * (1 + buff)).round.toInt } }
			}
		}
		
		// Returns the average score
		averageScore
	}
}
