package alankstewart.satin

import scala.math.log
import scala.math.BigDecimal._
import scala.math.BigDecimal.RoundingMode.HALF_UP

class Gaussian(val inputPower: Int, val outputPower: Double, val saturationIntensity: Int) {

    def logOutputPowerDividedByInputPower(): BigDecimal = {
        double2bigDecimal(log(outputPower / inputPower)).setScale(3, HALF_UP)
    }

    def outputPowerMinusInputPower(): BigDecimal = {
        (double2bigDecimal(outputPower) - int2bigDecimal(inputPower)).setScale(3, HALF_UP)
    }
}
