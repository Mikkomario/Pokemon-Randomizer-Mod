package vf.controller

import com.dabomstew.pkrandom.pokemon.Type
import utopia.paradigm.transform.Adjustment
import vf.model.{Poke, PokeStat, Pokes}
import vf.model.PokeStat._

import java.io.PrintWriter

/**
 * An algorithm that modifies poke stats in order to reflect the new type's "character"
 * @author Mikko Hilpinen
 * @since 28.11.2023, v1.0-alt
 */
object ReflectTypeChangesInStats
{
	private val changeAdjustment = Adjustment(0.14)
	private val staticAdjustment = Adjustment(0.07)
	
	private val typeStatImpact = Map[Type, Map[PokeStat, Int]](
		Type.NORMAL -> Map(
			Hp -> 2,
			Attack -> 1,
			SpecialAttack -> -2,
			SpecialDefense -> -1
		),
		Type.FLYING -> Map(
			Speed -> 3,
			Defense -> -1,
			SpecialDefense -> -1,
			SpecialAttack -> -1
		),
		Type.FIGHTING -> Map(
			Attack -> 3,
			SpecialAttack -> -2,
			SpecialDefense -> -1
		),
		Type.DARK -> Map(
			Speed -> 1,
			Attack -> 1,
			Defense -> -1,
			Hp -> -1
		),
		Type.PSYCHIC -> Map(
			SpecialAttack -> 3,
			SpecialDefense -> 2,
			Attack -> -3,
			Defense -> -2
		),
		Type.FAIRY -> Map(
			SpecialDefense -> 2,
			SpecialAttack -> 1,
			Speed -> -1,
			Defense -> -1,
			Attack -> -1
		),
		Type.GHOST -> Map(
			SpecialDefense -> 1,
			SpecialAttack -> 1,
			Hp -> -1,
			Speed -> -1
		),
		Type.BUG -> Map(
			Defense -> 1,
			Attack -> 1,
			SpecialDefense -> -1,
			SpecialAttack -> -1
		),
		Type.GROUND -> Map(
			Defense -> 2,
			Attack -> 2,
			SpecialDefense -> -2,
			SpecialAttack -> -2
		),
		Type.ROCK -> Map(
			Defense -> 2,
			Attack -> 2,
			Speed -> -3,
			SpecialAttack -> -1
		),
		Type.STEEL -> Map(
			Defense -> 3,
			Speed -> -3
		),
		Type.POISON -> Map(
			Defense -> 1,
			Speed -> -1
		),
		Type.GRASS -> Map(
			SpecialAttack -> 1,
			Speed -> -1
		),
		Type.WATER -> Map(
			Hp -> 1,
			SpecialDefense -> 1,
			Speed -> -1,
			Attack -> -1
		),
		Type.FIRE -> Map(
			Attack -> 1,
			SpecialAttack -> 1,
			Defense -> -1,
			SpecialDefense -> -1
		),
		Type.ELECTRIC -> Map(
			SpecialAttack -> 2,
			Speed -> 2,
			Defense -> -3,
			Hp -> -1
		)
	)
	
	def apply()(implicit pokes: Pokes): Unit = Log("type-stats") { writer => pokes.foreach { apply(_, writer) } }
	
	private def apply(poke: Poke, writer: PrintWriter) = {
		val stateBeforeChanges = poke.state
		// When types change, applies the effects in full
		poke.typeSwaps.foreach { case (fromType, toType) =>
			val fromImpact = typeStatImpact.getOrElse(fromType, Map())
			val toImpact = typeStatImpact.getOrElse(toType, Map())
			PokeStat.values.foreach { stat =>
				val change = toImpact.getOrElse(stat, 0) - fromImpact.getOrElse(stat, 0)
				if (change != 0)
					poke.mapStat(stat) { s => (s * changeAdjustment(change)).round.toInt }
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
		PokeStat.values.foreach { stat =>
			val before = stateBeforeChanges(stat)
			val after = poke(stat)
			if (before != after)
				writer.println(s"\t- $stat: $before => $after (${ if (after > before) "+" else "" }${
					((after - before) / before.toDouble * 100).round.toInt}%)")
		}
	}
}
