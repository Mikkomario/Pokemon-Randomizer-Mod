package vf.model

/**
 * An enumeration for different importance / strength levels on trainers
 */
sealed trait TrainerType

object TrainerType
{
	case object Regular extends TrainerType
	case object Important extends TrainerType
	case object Boss extends TrainerType
}
