package alankstewart.satin

case class Laser(outputFile: String, smallSignalGain: Double, dischargePressure: Int, carbonDioxide: String) {

  override def toString = s"$outputFile  $smallSignalGain  $dischargePressure  $carbonDioxide"
}
