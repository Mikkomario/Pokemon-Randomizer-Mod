package vf.controller

import com.dabomstew.pkrandom.pokemon.Type
import com.dabomstew.pkrandom.romhandlers.RomHandler
import vf.model.{EvolveGroup, Types}

/**
 * Tracks data collected during the randomization process
 * @author Mikko Hilpinen
 * @since 7.7.2023, v1.0-alt
 */
class JavaRandomizationContext(implicit romHandler: RomHandler)
{
	// ATTRIBUTES   ---------------------
	
	private implicit val originalTypes: Types = Types.from(romHandler.getPokemonInclFormes)
	private implicit val evolveGroups: Set[EvolveGroup] = EvolveGroup.all
	
	private var typeConversions = Map[Int, (Type, Type)]()
	private var typeAdditions = Map[Int, Type]()
	
	
	// OTHER    ------------------------
	
	def randomizeTypes() = {
		val (conversions, additions) = RandomizeTypes(evolveGroups)
		typeConversions = conversions
		typeAdditions = additions
	}
	
	def randomizeStats() = RandomizeStats.all()
}
