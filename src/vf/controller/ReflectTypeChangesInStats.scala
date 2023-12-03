package vf.controller

import utopia.paradigm.transform.Adjustment
import vf.model.{Poke, PokeStatAccess, Pokes}
import vf.poke.core.model.enumeration.{PokeType, Stat}
import vf.poke.core.model.enumeration.PokeType._
import vf.poke.core.model.enumeration.Stat._

import java.io.PrintWriter

/**
 * An algorithm that modifies poke stats in order to reflect the new type's "character"
 * @author Mikko Hilpinen
 * @since 28.11.2023, v1.0-alt
 */
object ReflectTypeChangesInStats
{
	private val leftTypeAdjustment = Adjustment(0.06)
	private val acquiredTypeAdjustment = Adjustment(0.12)
	private val staticAdjustment = Adjustment(0.04)
	
	private val typeStatImpact = Map[PokeType, Map[Stat, Int]](
		Normal -> Map(
			Hp -> 2,
			Attack -> 1,
			SpecialAttack -> -2,
			SpecialDefense -> -1
		),
		Flying -> Map(
			Speed -> 3,
			Defense -> -1,
			SpecialDefense -> -1,
			SpecialAttack -> -1
		),
		Fighting -> Map(
			Attack -> 3,
			SpecialAttack -> -2,
			SpecialDefense -> -1
		),
		Dark -> Map(
			Speed -> 1,
			Attack -> 1,
			Defense -> -1,
			Hp -> -1
		),
		Psychic -> Map(
			SpecialAttack -> 3,
			SpecialDefense -> 2,
			Attack -> -3,
			Defense -> -2
		),
		Fairy -> Map(
			SpecialDefense -> 2,
			SpecialAttack -> 1,
			Speed -> -1,
			Defense -> -1,
			Attack -> -1
		),
		Ghost -> Map(
			SpecialDefense -> 1,
			SpecialAttack -> 1,
			Hp -> -1,
			Speed -> -1
		),
		Bug -> Map(
			Defense -> 1,
			Attack -> 1,
			SpecialDefense -> -1,
			SpecialAttack -> -1
		),
		Ground -> Map(
			Defense -> 2,
			Attack -> 2,
			SpecialDefense -> -2,
			SpecialAttack -> -2
		),
		Rock -> Map(
			Defense -> 2,
			Attack -> 2,
			Speed -> -3,
			SpecialAttack -> -1
		),
		Steel -> Map(
			Defense -> 3,
			Speed -> -3
		),
		Poison -> Map(
			Defense -> 1,
			Speed -> -1
		),
		Grass -> Map(
			SpecialAttack -> 1,
			Speed -> -1
		),
		Water -> Map(
			Hp -> 1,
			SpecialDefense -> 1,
			Speed -> -1,
			Attack -> -1
		),
		Fire -> Map(
			Attack -> 1,
			SpecialAttack -> 1,
			Defense -> -1,
			SpecialDefense -> -1
		),
		Electric -> Map(
			SpecialAttack -> 2,
			Speed -> 2,
			Defense -> -3,
			Hp -> -1
		),
		Dragon -> Map()
	)
	
	def apply()(implicit pokes: Pokes): Unit = Log("type-stats") { writer => pokes.foreach { apply(_, writer) } }
	
	private def apply(poke: Poke, writer: PrintWriter) = {
		val stateBeforeChanges = poke.state
		// When types change, applies the effects in full
		poke.typeSwaps.foreach { case (fromType, toType) =>
			val fromImpact = typeStatImpact.getOrElse(fromType, Map())
			val toImpact = typeStatImpact.getOrElse(toType, Map())
			Stat.values.foreach { stat =>
				val fromEffect = leftTypeAdjustment(fromImpact.getOrElse(stat, 0).toDouble)
				val toEffect = acquiredTypeAdjustment(toImpact.getOrElse(stat, 0).toDouble)
				val totalEffect = fromEffect + toEffect
				if (totalEffect != 0)
					poke.mapStat(stat) { s => (s * totalEffect).round.toInt }
			}
		}
		// In addition, applies a small portion to the resulting types, regardless of whether they changed or not
		poke.types.types.foreach { t =>
			typeStatImpact.get(t).foreach { _.foreach { case (stat, impact) =>
				poke.mapStat(stat) { s => (s * staticAdjustment(impact)).round.toInt }
			} }
		}
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
