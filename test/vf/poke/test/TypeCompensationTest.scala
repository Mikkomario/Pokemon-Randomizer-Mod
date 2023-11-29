package vf.poke.test

/**
 * @author Mikko Hilpinen
 * @since 29.11.2023, v1.0-alt
 */
object TypeCompensationTest extends App
{
	// Mightyena (Dark): -395.0 vs. 660.0253759398496 (-159%)
	//	=> 0% buff
	// Houndoom(Dark | Fighting): 1595.0 vs.660.0253759398496(141 %)
	// => 0 % debuff
	val score = -324.0
	val averageScore = 660.0254
	val variance = (score - averageScore) / averageScore.abs
	val ignoredVariance = 0.25
	private val buffPer10PercentVariance = 0.015
	private val debuffPer10PercentVariance = 0.01
	
	println(s"$score vs. $averageScore (${(variance * 100).toInt}%)")
	if (variance > ignoredVariance) {
		val debuff = debuffPer10PercentVariance * (variance - ignoredVariance) / 0.1
		println(s"\t=> ${(debuff * 100).toInt}% debuff")
	}
	else if (variance < -ignoredVariance) {
		val buff = buffPer10PercentVariance * (variance.abs - ignoredVariance) / 0.1
		// println(s"$buff = $buffPer10PercentVariance * (${variance.abs} - $ignoredVariance) / 0.1")
		// println(s"$buff = $buffPer10PercentVariance * ${ (variance.abs - ignoredVariance) / 0.1 }")
		println(s"\t=> ${(buff * 100).toInt}% buff")
	}
}
