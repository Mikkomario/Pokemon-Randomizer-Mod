package vf.controller

import com.dabomstew.pkrandom.pokemon.{Evolution, Pokemon}
import com.dabomstew.pkrandom.romhandlers.RomHandler
import utopia.flow.collection.CollectionExtensions._
import vf.model.{EvolveGroup, Pokes}

import scala.jdk.CollectionConverters._

/**
 * Applies the "eviolite" mode to certain pokes.
 * @author Mikko Hilpinen
 * @since 30.11.2023, v1.0-alt
 */
object ProcessEviolites
{
	private val stageUnevolvedRate = 0.4
	private val defaultEvolveLevelIncrease = 10
	
	// Rate of how much the BST difference (between eviolite and evolved form) is "caught up" by the unevolved from
	private val evoStatCatchUpRate = 0.8
	// Rate of eviolite form stat increase applied to the previous form, if applicable
	private val preFormStatBoostMod = 0.3
	
	// The rate by how much the highest stat(s) of the eviolite poke outperform the evolved form
	private val outperformanceRates = Vector(0.2, 0.05)
	
	/**
	 * Detaches eviolite pokes from their evolve-chains. Should be called before Pokes and EvolveGroups are initiated.
	 * @param rom Implicit rom to modify
	 * @return Processed eviolite pokes and their detached evos
	 */
	def preprocess()(implicit rom: RomHandler) = {
		Log("eviolites-prepare") { writer =>
			rom.getPokemonInclFormes.iterator().asScala.filter { p => Settings.isEviolitePoke(p.name) }
				.map { poke =>
					// Removes the evolves from the eviolite poke, as well as from the evolved form
					// This will result in the forms becoming separate evolve-groups
					val toEvos = poke.evolutionsFrom.asScala.toVector
					writer.println(s"\nMaking ${poke.name} an eviolite poke by clearing evos to:")
					toEvos.foreach { evo => writer.println(s"\t- ${evo.to.name}") }
					poke.evolutionsFrom.clear()
					toEvos.foreach { _.to.evolutionsTo.clear() }
					
					// Changes the eviolite poke's own evolve level to later, unless its a basic form
					val wouldHaveEvolvedLevel = toEvos.filter { _.`type`.usesLevel() }.map { _.extraInfo }
						.filter { _ > 10 }.minOption
					poke.evolutionsTo.forEach { evo =>
						if (evo.`type`.usesLevel()) {
							val originalEvoLevel = evo.extraInfo
							val newEvolveLevel = wouldHaveEvolvedLevel match {
								// Level-based mid-stage evo => Proceeds a certain % of this stage before evolving
								case Some(removedEvoLevel) =>
									originalEvoLevel +
										((removedEvoLevel - originalEvoLevel) * stageUnevolvedRate).round.toInt
								// Case: The final form was not level-based
								case None => originalEvoLevel + defaultEvolveLevelIncrease
							}
							evo.extraInfo = newEvolveLevel
							writer.println(s"${evo.from.name} now evolves at lvl $newEvolveLevel")
						}
						else
							writer.print(s"Didn't modify evolve from ${evo.from.name}")
					}
					
					poke -> toEvos
				}
				.toMap
		}
	}
	
	def stats(eviolites: Map[Pokemon, Vector[Evolution]], evolveGroupsByNumber: Map[Int, EvolveGroup])
	         (implicit pokes: Pokes) =
	{
		Log("eviolites-apply") { writer =>
			eviolites.foreach { case (pokemon, evolved) =>
				val poke = pokes(pokemon)
				val evolveGroup = evolveGroupsByNumber(poke.number)
				val evolvedForms = evolved.map { evo => pokes(evo.to) }
				
				writer.println(s"\nProcessing ${poke.name} that would have evolved to ${
					evolvedForms.map { _.name }.mkString(" / ") }")
				
				// Determines the amount of BST boost to apply
				val bstDifferenceRate = evolvedForms.map { _.bst }.maxOption match {
					case Some(evolvedBst) => (evolvedBst - poke.bst) / poke.bst
					case None => 0.0
				}
				val bstScaling = 1.0 + bstDifferenceRate * evoStatCatchUpRate
				// Applies the BST scaling
				if (bstScaling > 1.0) {
					writer.println(s"\t- Increases all stats by ${ ((bstScaling - 1.0) * 100).round.toInt }%")
					poke.scaleStats(bstScaling)
					
					// May apply some scaling to the previous form(s) as well
					evolveGroup.iterator.takeWhile { _ != poke }.foreach { preForm =>
						writer.println(s"\t\t- Also increases ${ preForm.name } stats by ${
							((bstScaling - 1.0) * preFormStatBoostMod * 100).round.toInt }%")
						preForm.scaleStats(bstScaling * preFormStatBoostMod)
					}
				}
				
				// Applies the outperformance on the highest stats
				poke.stats.toVector.reverseSortBy { _._2 }.zip(outperformanceRates)
					.foreach { case ((stat, currentValue), outPerformanceRate) =>
						val newValue = (evolvedForms.map { _(stat) }.max * (1 + outPerformanceRate)).round.toInt
						if (newValue > currentValue) {
							val increase = newValue - currentValue
							writer.println(s"\t- Increases $stat by further $increase (${
								((increase / currentValue.toDouble) * 100).round.toInt }%) in order to out-perform the evolved form in this area by ${
								outPerformanceRate * 100 }%")
							poke(stat) = newValue
						}
					}
			}
		}
	}
}
