package alankstewart.satin

import scala.io.Source
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
        getInputPowers.foreach(println)
        getLaserData.foreach(println)
        // for (laser <- getLaserData) {
        //     println(laser)
        // }
        println(getOutputFilePath)
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
}
