package vf.model

import com.dabomstew.pkrandom.constants.GlobalConstants
import com.dabomstew.pkrandom.pokemon.{Move, MoveCategory}
import com.dabomstew.pkrandom.romhandlers.RomHandler

import scala.jdk.CollectionConverters._

object Moves
{
	def inGame(rom: RomHandler) = Moves(
		rom.getMoves.iterator().asScala.flatMap { Option(_) }.toSet,
		rom.getGameBreakingMoves.iterator().asScala.flatMap { Option[Int](_) }.toSet ++
			rom.getHMMoves.iterator().asScala.flatMap { Option[Int](_) } ++
			rom.getMovesBannedFromLevelup.iterator().asScala.flatMap { Option[Int](_) } ++
			GlobalConstants.zMoves.iterator().asScala.flatMap { Option[Int](_) } ++
			rom.getIllegalMoves.iterator().asScala.flatMap { Option[Int](_) }
	)
}

/**
 * Contains information about all moves in a game
 * @author Mikko Hilpinen
 * @since 8.7.2023, v1.0-alt
 */
case class Moves(all: Set[Move], banned: Set[Int])
{
	lazy val valid = all.filterNot { m => banned.contains(m.number) }
	
	lazy val byNumber = all.iterator.map { m => m.number -> m }.toMap
	
	lazy val byType = valid.groupBy { _.`type` }.withDefaultValue(Set())
	lazy val byHasStatChange = valid.groupBy { _.hasBeneficialStatChange }.withDefaultValue(Set())
	lazy val byDamages = valid.groupBy { _.power > 0 }.withDefaultValue(Set())
	lazy val byCategory = valid.groupBy { _.category }.withDefaultValue(Set())
	
	lazy val statusMoveRatioByType = byType.view
		.mapValues { moves => moves.count { _.category == MoveCategory.STATUS }.toDouble / moves.size }
		.toMap.withDefaultValue(0.0)
}