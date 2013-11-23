package alankstewart.satin

import scala.io.Source
import scala.collection.mutable.ListBuffer
import scalax.file.Path
import scala.math.BigDecimal._
import scala.math.BigDecimal.RoundingMode.HALF_UP
import scala.collection.mutable.ListBuffer
import com.github.nscala_time.time.Imports._
import com.typesafe.config._

object Satin {

  object CO2 extends Enumeration {
    val MD, PI = Value
  }

  class Laser(val outputFile: String, val smallSignalGain: Float, val dischargePressure: Int,
    val carbonDioxide: CO2.Value) {
    override def toString() = outputFile + "  " + smallSignalGain + "  " + dischargePressure + "  " + carbonDioxide
  }

  def main(args: Array[String]) {
    println("Hello, world!")
    calculate
  }

  def calculate() {
    var total = 0
    val inputPowers: List[Int] = getInputPowers
    getLaserData.foreach(laser => {
      println("processing " + laser.outputFile)
      val gaussianData = new ListBuffer[Gaussian]()
      var count = 0
      inputPowers.foreach(inputPower => {
        gaussianData ++= new GaussianLaserBean(inputPower, laser.smallSignalGain).calculateGaussians.to[ListBuffer]
        count += 1
      })
      writeToFile(laser, gaussianData.toList)
      total += count
    })
    println("total = " + total)
  }

  def getInputPowers: List[Int] = {
    val source = Source.fromFile(getDataFilePath + "pin.dat")
    val inputPowers = source.getLines.map(line => line.trim.toInt).toList
    source.close()
    return inputPowers
  }

  def getLaserData: List[Laser] = {
    val source = Source.fromFile(getDataFilePath + "laser.dat")
    val laserData = source.getLines.map(line => {
      val tokens = line.split("  ")
      new Laser(tokens(0), tokens(1).trim.toFloat, tokens(2).trim.toInt, CO2.withName(tokens(3).trim))
    }).toList
    source.close
    return laserData
  }

  def getDataFilePath: String = {
    ConfigFactory.load().getString("dataFilePath")
  }

  def getOutputFilePath: String = {
    ConfigFactory.load().getString("outputFilePath")
  }
  
  def writeToFile(laser: Laser, gaussianData: List[Gaussian]) = {
    val path: Path = Path.fromString(getOutputFilePath + laser.outputFile).createFile(failIfExists = false)
    val lines = new ListBuffer[String] 
    lines += "Start date: %s\n\nGaussian Beam\n\nPressure in Main Discharge = %dkPa\nSmall-signal Gain = %4.1f\nCO2 via %s\n\nPin\t\tPout\t\tSat. Int\tln(Pout/Pin)\tPout-Pin\n(watts)\t\t(watts)\t\t(watts/cm2)\t\t\t(watts)\n"
      .format(DateTime.now, laser.dischargePressure, laser.smallSignalGain, laser.carbonDioxide)      
    lines ++= gaussianData.map((gaussian:Gaussian) => "%s\t\t%s\t\t%s\t\t%s\t\t%s\n".format(gaussian.inputPower,
      double2bigDecimal(gaussian.outputPower).setScale(3, HALF_UP), gaussian.saturationIntensity, gaussian.logOutputPowerDividedByInputPower, gaussian.outputPowerMinusInputPower) 
      ).toList.to[ListBuffer]
    lines += "\nEnd date: %s\n".format(DateTime.now)
    path.writeStrings(lines.toList, "")
  }
}
