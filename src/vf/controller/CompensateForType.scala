package vf.controller

import utopia.flow.collection.CollectionExtensions._
import utopia.flow.collection.immutable.Pair
import vf.model.{EffectivenessRelations, EvolveGroup, Poke, TypeSet}
import vf.poke.core.model.enumeration.Stat
import vf.poke.core.model.enumeration.Stat._

import java.io.PrintWriter

/**
 * An algorithm for compensating for weaker and stronger types in the game
 * @author Mikko Hilpinen
 * @since 28.11.2023, v1.0-alt
 */
object CompensateForType
{
	private val ignoredVariance = 0.25
	private val buffPer10PercentVariance = 0.012
	private val debuffPer10PercentVariance = 0.008

	/**
	 * Adjusts some poke's offensive and defensive stats in order to compensate for their relatively
	 * weak or strong types
	 * @param minAppearanceLevels The lowest levels at which each encountered evolve group may be caught
	 * @param encounterCounts Number of encounters per level for each poke (that has an encounter)
	 */
	def apply(minAppearanceLevels: Map[EvolveGroup, Int], encounterCounts: Map[Poke, Map[Int, Int]]): Unit = {
		Log("type-strength-compensation") { writer =>
			val orderedEncounterCounts = encounterCounts.flatMap { case (poke, levelCounts) =>
				levelCounts.map { case (level, count) => (poke.types, level, count) }
			}.toVector.reverseSortBy { _._2 }
			// Calculates the defensive type-based score of every poke against every enemy,
			// summing the score together to an average score
			writer.println("Applies defensive buffs & debuffs")
			/*val averageDefenseScore = */apply(minAppearanceLevels, orderedEncounterCounts,
				Pair(Defense, SpecialDefense), writer) { _.defenseRatingAgainst(_) }
			// Calculates a similar score in terms of offense
			writer.println("Applies offensive buffs & debuffs")
			/*val averageAttackScore = */apply(minAppearanceLevels, orderedEncounterCounts,
				Pair(Attack, SpecialAttack), writer) { _.offenseRatingAgainst(_) }
			
			// Logs the applied changes
			/*
			writer.println("\nApplies the following type-specific buffs & debuffs:")
			pokePool.map { p: Poke => p.types }.toSet.foreach { types: TypeSet =>
				writer.println(s"\t- $types")
				val effectiveness = types.effectiveness
				// Calculates the defensive and offensive scores for the type
				val scores = encounterCounts.iterator
					.map { case (opponent, encounters) =>
						val defensive = effectiveness.defenseRatingAgainst(opponent.types)
						val offensive = effectiveness.offenseRatingAgainst(opponent.types)
						Pair(defensive, offensive).map { _ * encounters }
					}
					.reduce { _.mergeWith(_) { _ + _ } }
				// Calculates a score for every pokemon, and the average score
				val variances = scores.mergeWith(Pair(averageDefenseScore, averageAttackScore)) { (score, average) =>
					(score - average) / average.abs
				}
				variances.mergeWith(Pair("Defense", "Offense")) { (variance, title) =>
					if (variance > ignoredVariance) {
						val debuff = debuffPer10PercentVariance * (variance - ignoredVariance) / 0.1
						writer.println(s"\t\t- $title: -${ (debuff * 100).round.toInt }%")
					}
					else if (variance < -ignoredVariance) {
						val buff = buffPer10PercentVariance * (variance.abs - ignoredVariance) / 0.1
						writer.println(s"\t\t- $title: +${ (buff * 100).round.toInt }%")
					}
					else
						writer.println(s"\t\t- $title: No change")
				}
			}
			 */
		}
	}
	
	// Encounter counts must be sorted according to encounter level in descending order
	// Each entry contains 1) Opponent types, 2) Opponent level and 3) Number of similar encounters
	private def apply(minAppearanceLevels: Map[EvolveGroup, Int], encounterCounts: Vector[(TypeSet, Int, Int)],
	                  affectedStats: Pair[Stat], writer: PrintWriter)
	                 (scoreFunction: (EffectivenessRelations, TypeSet) => Double) =
	{
		// Calculates a score for every pokemon, and the average score
		val scorePerPoke = minAppearanceLevels.flatMap { case (group, firstAppearanceLevel) =>
			group.levelThresholds.flatMap { case (poke, evolveLevel) =>
				val firstLevel = firstAppearanceLevel max evolveLevel
				val applicableEncounterCounts = encounterCounts.takeWhile { _._2 >= firstLevel }
					.groupMapReduce { _._1 } { _._3 } { _ + _ }
				if (applicableEncounterCounts.isEmpty)
					None
				else {
					val effectiveness = poke.types.effectiveness
					val totalScore = applicableEncounterCounts.map { case (opponentTypes, count) =>
						scoreFunction(effectiveness, opponentTypes) * count
					}.sum
					val scorePerEncounter = totalScore / applicableEncounterCounts.valuesIterator.sum
					Some(poke -> scorePerEncounter)
				}
			}
		}
		val averageScore = scorePerPoke.valuesIterator.sum / scorePerPoke.size
		
		// Adjusts every pokemon according to their score
		scorePerPoke.foreach { case (poke, score) =>
			val variance = (score - averageScore) / averageScore.abs
			writer.println(s"${poke.name} (${poke.types}): $score vs. $averageScore (${(variance * 100).toInt}%)")
			if (variance > ignoredVariance) {
				val debuff = debuffPer10PercentVariance * (variance - ignoredVariance) / 0.1
				writer.println(s"\t=> ${(debuff * 100).toInt}% debuff")
				affectedStats.foreach { poke.mapStat(_) { s => (s * (1 - debuff)).round.toInt } }
			}
			else if (variance < -ignoredVariance) {
				val buff = buffPer10PercentVariance * (variance.abs - ignoredVariance) / 0.1
				writer.println(s"\t=> ${(buff * 100).toInt}% buff")
				affectedStats.foreach { poke.mapStat(_) { s => (s * (1 + buff)).round.toInt } }
			}
		}
		
		// Returns the average score
		averageScore
	}
}
