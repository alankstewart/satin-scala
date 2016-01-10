package alankstewart.satin

case class Laser(outputFile: String, smallSignalGain: Double, dischargePressure: Int, carbonDioxide: String) {

  override def toString = outputFile + "  " + smallSignalGain + "  " + dischargePressure + "  " + carbonDioxide
}
