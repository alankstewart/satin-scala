package alankstewart.satin

object CO2 extends Enumeration {
  val MD, PI = Value
}

case class Laser(outputFile: String, smallSignalGain: Double, dischargePressure: Int, carbonDioxide: CO2.Value) {

  override def toString = outputFile + "  " + smallSignalGain + "  " + dischargePressure + "  " + carbonDioxide
}

