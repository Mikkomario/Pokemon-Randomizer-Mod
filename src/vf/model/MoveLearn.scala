package vf.model

import com.dabomstew.pkrandom.pokemon.MoveLearnt

object MoveLearn
{
	def from(m: MoveLearnt) = apply(m.level, m.move)
	
	def evo(move: Int) = apply(0, move)
	def start(move: Int) = apply(1, move)
}

/**
 * An immutable model that represents an event when a poke learns a move
 * @author Mikko Hilpinen
 * @since 26.11.2023, v1.0-alt
 */
case class MoveLearn(level: Int, move: Int)
{
	// COMPUTED --------------------
	
	def toMoveLearnt = {
		val l = new MoveLearnt()
		l.level = level
		l.move = move
		l
	}
	
	def isEvoMove = level < 0
	def isStartingMove = level == 0
	def isLearnedMove = level > 1
	
	
	// OTHER    --------------------
	
	def withMove(move: Int) = copy(move = move)
	def mapMove(f: Int => Int) = withMove(f(move))
}
