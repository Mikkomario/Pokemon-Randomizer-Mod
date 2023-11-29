package vf.model

/**
 * An enumeration for different wild encounter area types, in places where such may be discerned
 * @author Mikko Hilpinen
 * @since 15.7.2023, v1.0-alt
 */
sealed trait WildEncounterType
{
	// def primaryTypes: Set[Type]
	// TODO: Add secondary types, also. And "impact strength" (low for ambiguous area types)
}

object WildEncounterType
{
	case object GrassOrCave extends WildEncounterType
	{
		// override def primaryTypes: Set[Type] = Set(Type.GRASS, Type.GROUND, Type.NORMAL)
	}
	case object Grass extends WildEncounterType
	{
		// override def primaryTypes: Set[Type] = Set(Type.GRASS, Type.BUG)
	}
	case object Surf extends WildEncounterType
	{
		// override def primaryTypes: Set[Type] = ???
	}
	case object Fishing extends WildEncounterType
	{
		// override def primaryTypes: Set[Type] = ???
	}
	case object Rocks extends WildEncounterType
	{
		// override def primaryTypes: Set[Type] = ???
	}
	case object Bugs extends WildEncounterType
	{
		// override def primaryTypes: Set[Type] = ???
	}
	case object Trees extends WildEncounterType
	{
		// override def primaryTypes: Set[Type] = ???
	}
	case object Flowers extends WildEncounterType
	{
		// override def primaryTypes: Set[Type] = ???
	}
	case object Horde extends WildEncounterType
	{
		// override def primaryTypes: Set[Type] = ???
	}
}