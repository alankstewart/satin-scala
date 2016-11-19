package alankstewart.satin

import scala.math.log
import scala.math.BigDecimal._
import scala.math.BigDecimal.RoundingMode.HALF_UP

case class Gaussian(inputPower: Int, outputPower: Double, saturationIntensity: Int) {

  def logOutputPowerDividedByInputPower() =
    double2bigDecimal(log(outputPower / inputPower)).setScale(3, HALF_UP)

  def outputPowerMinusInputPower() =
    (double2bigDecimal(outputPower) - int2bigDecimal(inputPower)).setScale(3, HALF_UP)
}
