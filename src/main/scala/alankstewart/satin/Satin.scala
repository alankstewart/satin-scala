package alankstewart.satin

import java.io._
import java.time.LocalDateTime.now
import java.time.format.DateTimeFormatter

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.{Duration, SECONDS}
import scala.concurrent.{Await, Future}
import scala.io.Source
import scala.math.BigDecimal.RoundingMode.HALF_UP
import scala.math.BigDecimal._
import scala.math.{Pi, exp, pow}

object Satin extends App {

  val Path = System.getProperty("user.dir")
  val DateFormatter = DateTimeFormatter.ofPattern("d MMM yyyy HH:mm:ss.SSS")
  val Rad = 0.18
  val W1 = 0.3
  val Dr = 0.002
  val Dz = 0.04
  val Lambda = 0.0106
  val Area = Pi * pow(Rad, 2)
  val Z1 = Pi * pow(W1, 2) / Lambda
  val Z12 = Z1 * Z1
  val Expr = 2 * Pi * Dr
  val Incr = 8001

  val start = System.nanoTime
  try {
    if (args.length > 0 && args(0).equals("-single")) {
      calculateConcurrently
    } else {
      Await.result(Future.sequence(calculateConcurrently), Duration(60, SECONDS))
    }
  } catch {
    case t: Throwable => println(s"Failed to complete: $t")
  } finally {
    println(s"The time was ${
      (long2bigDecimal(System.nanoTime - start) / double2bigDecimal(1E9)).setScale(3, HALF_UP)
    } seconds")
  }

  def calculate() = {
    val inputPowers = getInputPowers
    getLaserData.foreach(laser => process(inputPowers, laser))
  }

  def calculateConcurrently = {
    val inputPowers = getInputPowers
    val laserData = getLaserData
    for (laser <- laserData) yield Future {
      process(inputPowers, laser)
    }
  }

  private def getInputPowers = {
    val source = Source.fromURI(getClass.getClassLoader.getResource("pin.dat").toURI)
    try source.getLines.map(_.trim.toInt).toArray finally source.close
  }

  private def getLaserData = {
    val pattern = "((md|pi)[a-z]{2}\\.out)\\s+([0-9]{2}\\.[0-9])\\s+([0-9]+)\\s+(?i:\\2)".r
    val source = Source.fromURI(getClass.getClassLoader.getResource("laser.dat").toURI)
    try source.getLines.map(line => pattern.findFirstMatchIn(line)
      .map(m => Laser(m.group(1), m.group(3).toDouble, m.group(4).toInt, m.group(2))).get)
      .toList finally source.close
  }

  private def process(inputPowers: Array[Int], laser: Laser) {
    val path = new PrintWriter(new File(Path + "/" + laser.outputFile))
    path.write(
      f"""
         |Start date: ${now.format(DateFormatter)}
         |
         |Gaussian Beam
         |
         |Pressure in Main Discharge = ${laser.dischargePressure}kPa
         |Small-signal Gain = ${laser.smallSignalGain}%4.1f
         |CO2 via ${laser.carbonDioxide}
         |
         |Pin		Pout		Sat. Int	ln(Pout/Pin)	Pout-Pin
         |(watts)		(watts)		(watts/cm2)			(watts)""".stripMargin
    )

    inputPowers.foreach(inputPower => gaussianCalculation(inputPower, laser.smallSignalGain)
      .foreach(gaussian => path.write(
        s"""
           |${gaussian.inputPower}		${double2bigDecimal(gaussian.outputPower).setScale(3, HALF_UP)}		${gaussian.saturationIntensity}		${gaussian.logOutputPowerDividedByInputPower()}		${gaussian.outputPowerMinusInputPower()}""".stripMargin
      )))

    path.write(
      s"""
         |
         |End date: ${now.format(DateFormatter)}
         |""".stripMargin
    )
    path.close()
  }

  def gaussianCalculation(inputPower: Int, smallSignalGain: Double) = {
    val expr1 = Range(0, Incr).map(i => {
      val zInc = (i.toDouble - Incr / 2) / 25
      2 * zInc * Dz / (Z12 + pow(zInc, 2))
    }).toArray
    val inputIntensity = 2 * inputPower / Area
    val expr2 = smallSignalGain / 32000 * Dz

    Range.inclusive(10000, 25000, 1000).map(saturationIntensity => {
      val expr3 = saturationIntensity * expr2
      val outputPower = Range.Double.inclusive(0.0, 0.5, Dr).map(r => {
        var outputIntensity = inputIntensity * exp(-2 * pow(r, 2) / pow(Rad, 2))
        Range(0, Incr).foreach(j => {
          outputIntensity *= (1 + expr3 / (saturationIntensity + outputIntensity) - expr1(j))
        })
        outputIntensity * Expr * r
      }).sum
      Gaussian(inputPower, outputPower, saturationIntensity)
    }).toList
  }
}
