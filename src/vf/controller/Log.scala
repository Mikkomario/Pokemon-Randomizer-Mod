package vf.controller

import utopia.flow.parse.file.FileExtensions._
import utopia.flow.time.Today
import utopia.flow.util.StringExtensions._
import utopia.flow.view.mutable.eventful.SettableOnce

import java.io.PrintWriter
import java.nio.file.Path

/**
 * Interface for logging tools
 * @author Mikko Hilpinen
 * @since 27.11.2023, v1.0-alt
 */
object Log
{
	/**
	 * The logging directory
	 */
	val dir: Path = "data/log"
	
	def apply[U](fileName: String)(f: PrintWriter => U) = {
		val resultPointer = SettableOnce[U]()
		(dir/fileName.endingWith(".txt").startingWith(s"$Today-")).createDirectories()
			.flatMap { _.writeUsing { writer =>
				resultPointer.value = Some(f(writer))
			} }
			.get
		resultPointer.value.get
	}
}
