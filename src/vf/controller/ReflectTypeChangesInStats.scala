package vf.controller

import utopia.flow.collection.immutable.Pair
import utopia.flow.collection.CollectionExtensions._
import utopia.paradigm.transform.Adjustment
import vf.model.{Poke, Pokes}
import vf.poke.core.model.enumeration.PokeType._
import vf.poke.core.model.enumeration.Stat._
import vf.poke.core.model.enumeration.{PokeType, Stat}

import java.io.PrintWriter

/**
 * An algorithm that modifies poke stats in order to reflect the new type's "character"
 * @author Mikko Hilpinen
 * @since 28.11.2023, v1.0-alt
 */
object ReflectTypeChangesInStats
{
	/*
	private val leftTypeAdjustment = Adjustment(0.06)
	private val acquiredTypeAdjustment = Adjustment(0.12)
	private val staticAdjustment = Adjustment(0.04)
	*/
	private val leftTypeImpactMod = 0.5
	private val preservedTypeImpactMod = 0.25
	private val changeAdjustment = Adjustment(0.12)
	
	// HP: 0
	// Def: -2
	// SpDef: -1
	// Att: +2
	// SpAtt: +1
	// Speed: 0
	private val typeStatImpact = Map[PokeType, Map[Stat, Int]](
		Normal -> Map(
			Hp -> 3,
			SpecialAttack -> -2,
			SpecialDefense -> -1
		),
		Flying -> Map(
			Speed -> 5,
			Defense -> -3,
			SpecialDefense -> -1,
			SpecialAttack -> -1
		),
		Fighting -> Map(
			Attack -> 4,
			Defense -> 1,
			Speed -> 1,
			SpecialAttack -> -3,
			SpecialDefense -> -3
		),
		Dark -> Map(
			Speed -> 2,
			Defense -> -1,
			Hp -> -1
		),
		Psychic -> Map(
			SpecialAttack -> 4,
			SpecialDefense -> 3,
			Attack -> -3,
			Defense -> -3,
			Hp -> -1
		),
		Fairy -> Map(
			SpecialDefense -> 3,
			SpecialAttack -> 1,
			Attack -> -3,
			Defense -> -1
		),
		Ghost -> Map(
			SpecialDefense -> 1,
			SpecialAttack -> 2,
			Hp -> -1,
			Speed -> -1,
			Attack -> -1
		),
		Bug -> Map(
			Defense -> 2,
			SpecialAttack -> -1,
			Speed -> -1
		),
		Ground -> Map(
			Defense -> 2,
			Attack -> 1,
			SpecialDefense -> -1,
			SpecialAttack -> -2
		),
		Rock -> Map(
			Defense -> 3,
			Attack -> 2,
			Speed -> -3,
			SpecialAttack -> -2
		),
		Steel -> Map(
			Defense -> 4,
			Speed -> -3,
			SpecialAttack -> -1
		),
		Poison -> Map(
			Defense -> 1,
			Speed -> -1
		),
		Grass -> Map(
			SpecialAttack -> 1,
			Hp -> 1,
			Speed -> -2
		),
		Water -> Map(
			Hp -> 1,
			SpecialDefense -> 1,
			Speed -> -1,
			Defense -> -1
		),
		Ice -> Map(
			SpecialAttack -> 2,
			Attack -> 1,
			Defense -> -1,
			SpecialDefense -> -2
		),
		Fire -> Map(
			Attack -> 1,
			SpecialAttack -> 2,
			Speed -> 1,
			Defense -> -2,
			SpecialDefense -> -2
		),
		Electric -> Map(
			SpecialAttack -> 1,
			Speed -> 4,
			Defense -> -3,
			Hp -> -2
		),
		Dragon -> Map(
			SpecialDefense -> 1,
			Speed -> -1
		)
	)
	
	def apply()(implicit pokes: Pokes): Unit = Log("type-stats") { writer => pokes.foreach { apply(_, writer) } }
	
	private def apply(poke: Poke, writer: PrintWriter) = {
		val stateBeforeChanges = poke.state
		// When types change, applies the effects in full
		(poke.typeSwaps.map { case (fromType, toType) =>
			// Calculates the effect to apply to the stats
			// The effect is stronger for the gained type and lesser (and negative) for the original type
			Pair(fromType -> -leftTypeImpactMod, toType -> 1.0).mapAndMerge { case (t, impactMod) =>
				typeStatImpact.getOrElse(t, Map()).view.mapValues { _ * impactMod }.toMap
			} { _.mergeWith(_) { _ + _ } }
		} ++
			// Applies a smaller change for preserved types
			poke.preservedOriginalTypes
				.map { t => typeStatImpact.getOrElse(t, Map()).view.mapValues { _ * preservedTypeImpactMod }.toMap } ++
			// Gained types apply the full increase
			poke.addedType
				.map { gainedType => typeStatImpact.getOrElse(gainedType, Map()).view.mapValues { _.toDouble }.toMap }
			)
			// Combines the changes in order to get to the total
			.reduceOption { _.mergeWith(_) { _ + _ } }
			.foreach { changes =>
				// Reduces the stats first, and calculates how many stats are taken away
				val totalStatReduction = changes.filter { _._2 < 0 }.map { case (stat, impact) =>
					// Extracts based on the original stat values
					val originalStat = poke.originalState(stat)
					val decrease = originalStat - (changeAdjustment(impact) * originalStat).round.toInt
					poke.mapStat(stat) { _ - decrease }
					decrease
				}.sum
				// Assigns those stats between the positively affected stats, based on the impact distribution
				val positiveChanges = changes.filter { _._2 > 0 }
				val totalPositiveImpact = positiveChanges.valuesIterator.sum
				positiveChanges.foreach { case (stat, impact) =>
					poke.mapStat(stat) { _ + ((impact / totalPositiveImpact) * totalStatReduction).round.toInt }
				}
			}
		/*
		poke.typeSwaps.foreach { case (fromType, toType) =>
			val fromImpact = typeStatImpact.getOrElse(fromType, Map())
			val toImpact = typeStatImpact.getOrElse(toType, Map())
			
			Stat.values.foreach { stat =>
				val fromEffect = leftTypeAdjustment(-fromImpact.getOrElse(stat, 0).toDouble)
				val toEffect = acquiredTypeAdjustment(toImpact.getOrElse(stat, 0).toDouble)
				val totalEffect = fromEffect * toEffect
				if (totalEffect != 0)
					poke.mapStat(stat) { s => (s * totalEffect).round.toInt }
			}
		}
		*/
		/*
		// In addition, applies a small portion to the resulting types, regardless of whether they changed or not
		poke.types.types.foreach { t =>
			typeStatImpact.get(t).foreach { _.foreach { case (stat, impact) =>
				poke.mapStat(stat) { s => (s * staticAdjustment(impact)).round.toInt }
			} }
		}*/
		// Logs the changes
		writer.println(s"\n${poke.name}: ${poke.originalState.types} => ${poke.types}; ${
			stateBeforeChanges.bst } => ${poke.bst}")
		Stat.values.foreach { stat =>
			val before = stateBeforeChanges(stat)
			val after = poke(stat)
			if (before != after)
				writer.println(s"\t- $stat: $before => $after (${ if (after > before) "+" else "" }${
					((after - before) / before.toDouble * 100).round.toInt}%)")
		}
	}
}
