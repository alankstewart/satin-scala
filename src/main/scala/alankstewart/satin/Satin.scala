package alankstewart.satin

import scala.collection.mutable.ListBuffer
import scala.concurrent.Await
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.concurrent.duration.Duration
import scala.concurrent.duration.SECONDS
import scala.io.Source
import scala.math.BigDecimal._
import scala.math.BigDecimal.RoundingMode.HALF_UP
import scala.math.exp
import scala.math.Pi
import scala.math.pow

import java.io._
import java.util.Calendar

object Satin {

  val Rad = 0.18f
  val W1 = 0.3f
  val Dr = 0.002f
  val Dz = 0.04f
  val Lamda = 0.0106f
  val Area = Pi * pow(Rad, 2)
  val Z1 = Pi * pow(W1, 2) / Lamda
  val Z12 = Z1 * Z1
  val Expr = 2 * Pi * Dr
  val Incr = 8001

  def main(args: Array[String]) {
    val start: Long = System.nanoTime
    if (args.length > 0 && args(0).equals("-concurrent")) {
      calculateConcurrently
    } else {
      calculate
    }
    println("The time was %s seconds".format((long2bigDecimal(System.nanoTime - start) / double2bigDecimal(1E9)).setScale(3, HALF_UP)))
  }

  def calculateConcurrently(): Unit = {
    val inputPowers: List[Int] = getInputPowers
    val laserData: List[Laser] = getLaserData

    val tasks: Seq[Future[Unit]] = for (laser <- laserData) yield Future {
      process(inputPowers, laser)
    }
    Await.result(Future.sequence(tasks), Duration(60, SECONDS))
  }

  def calculate(): Unit = {
    val inputPowers: List[Int] = getInputPowers
    val laserData: List[Laser] = getLaserData

    laserData.foreach(laser => process(inputPowers, laser))
  }

  def getInputPowers: List[Int] = {
    readDataFile("/pin.dat").map(line => line.trim.toInt).toList
  }

  def getLaserData: List[Laser] = {
    readDataFile("/laser.dat").map(line => line.split("  ")).map(createLaser _).toList
  }

  def createLaser(tokens: Array[String]) = {
    new Laser(tokens(0), tokens(1).trim.toFloat, tokens(2).trim.toInt, CO2.withName(tokens(3).trim))
  }

  def process(inputPowers: List[Int], laser: Laser): Unit = {
    val path = new PrintWriter(new File(System.getProperty("user.home") + "/tmp/" + laser.outputFile));

    path.write(("Start date: %s\n\nGaussian Beam\n\nPressure in Main Discharge = %dkPa\nSmall-signal Gain = %4.1f\nCO2 via %s\n\nPin\t\tPout\t\tSat. Int\tln(Pout/Pin)\tPout-Pin\n(watts)\t\t(watts)\t\t(watts/cm2)\t\t\t(watts)\n")
      .format(Calendar.getInstance.getTime, laser.dischargePressure, laser.smallSignalGain, laser.carbonDioxide))

    inputPowers.foreach(inputPower => gaussianCalculation(inputPower, laser.smallSignalGain)
      .foreach(gaussian => path.write("%s\t\t%s\t\t%s\t\t%s\t\t%s\n"
      .format(gaussian.inputPower, double2bigDecimal(gaussian.outputPower).setScale(3, HALF_UP), gaussian
      .saturationIntensity, gaussian.logOutputPowerDividedByInputPower, gaussian.outputPowerMinusInputPower))))

    path.write("\nEnd date: %s\n".format(Calendar.getInstance.getTime))
    path.close
  }

  def gaussianCalculation(inputPower: Int, smallSignalGain: Float): List[Gaussian] = {
    val gaussians = new ListBuffer[Gaussian]()

    val expr1 = new Array[Double](Incr)
    for (i <- 0 until Incr) {
      val zInc = (i.toDouble - Incr / 2) / 25
      expr1(i) = 2 * zInc * Dz / (Z12 + pow(zInc, 2))
    }

    val inputIntensity = 2 * inputPower / Area
    val expr2 = (smallSignalGain / 32E3) * Dz

    for (saturationIntensity <- 10000 to 25000 by 1000) {
      var outputPower = 0.0; val expr3 = saturationIntensity * expr2
      for (r <- 0.0f to 0.5f by Dr) {
        var outputIntensity = inputIntensity * exp(-2 * pow(r, 2) / pow(Rad, 2))
        for (j <- 0 until Incr) {
          outputIntensity *= (1 + expr3 / (saturationIntensity + outputIntensity) - expr1(j));
        }
        outputPower += (outputIntensity * Expr * r);
      }
      gaussians += new Gaussian(inputPower, outputPower, saturationIntensity)
    }

    gaussians.toList
  }

  def readDataFile(name: String): List[String] = {
    val source = Source.fromInputStream(getClass.getResourceAsStream(name))
    try {
      source.getLines.toList
    } finally {
      source.close
    }
  }
}
