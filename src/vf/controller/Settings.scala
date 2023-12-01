package vf.controller

import utopia.flow.parse.file.FileExtensions._
import utopia.flow.parse.json.{JsonParser, JsonReader, JsonSettingsAccess}
import utopia.flow.util.StringExtensions._
import utopia.flow.util.logging.{Logger, SysErrLogger}
import vf.model.{EvolveGroup, Poke}

/**
 * An interface used for reading custom settings
 * @author Mikko Hilpinen
 * @since 30.11.2023, v1.0-alt
 */
object Settings
{
	// ATTRIBUTES   -------------------------
	
	private implicit val log: Logger = SysErrLogger
	private implicit val jsonParser: JsonParser = JsonReader
	private val access = new JsonSettingsAccess("data")
	
	private lazy val favoritePokeNames = access("favorites").getVector.flatMap { _.string }
		.map { pokeName =>
			val level = pokeName.optionIndexOf("+") match {
				case Some(suffixIndex) => 1 + pokeName.length - suffixIndex
				case None => 1
			}
			pokeName.toLowerCase -> level
		}
		.toMap
	private lazy val eviolitePokeNames = access("eviolites").getVector.flatMap { _.string }.map { _.toLowerCase }.toSet
	
	
	// OTHER    -------------------------
	
	/**
	 * @param pokes Poke group to evaluate
	 * @return Level of "favouriteness" of the specified poke (group)
	 */
	def favouriteLevelOf(pokes: EvolveGroup) = pokes.iterator
		.flatMap { p => favoritePokeNames.get(p.name.toLowerCase) }
		.maxOption.getOrElse(0)
	
	def isEviolitePoke(pokeName: String) = eviolitePokeNames.contains(pokeName.toLowerCase)
	def isEviolitePoke(poke: Poke): Boolean = isEviolitePoke(poke.name)
}
