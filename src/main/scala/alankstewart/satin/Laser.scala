package alankstewart.satin

object CO2 extends Enumeration {
  val MD, PI = Value
}

class Laser(val outputFile: String, val smallSignalGain: Double, val dischargePressure: Int, val carbonDioxide: CO2.Value) {

  override def toString = outputFile + "  " + smallSignalGain + "  " + dischargePressure + "  " + carbonDioxide
}

