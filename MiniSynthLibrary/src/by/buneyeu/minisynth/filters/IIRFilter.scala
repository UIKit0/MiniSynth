package by.buneyeu.minisynth.filters

import by.buneyeu.minisynth.SampleProcessor
import by.buneyeu.minisynth.SampleRateDevice
import by.buneyeu.minisynth.Implicits._

//TODO check it carefully and refactor hard in scala way
class IIRFilter(a: Array[Double], b: Array[Double]) extends SampleProcessor {

  require(b.length > 0, "To create IIR filter you should define at least one b coeff!")

  private val x = Array.fill(b.length)(0.0)
  private val y = Array.fill(a.length)(0.0)

  def processSample(xIn: Double): Double = {
    delay(x, xIn)
    val yOut = sum(b * x ++ a * y)
    delay(y, yOut)
    yOut
  }

  private def delay(arr: Array[Double], newValue: Double) = {
    //very stupid delay implementation with O(n) complexity,
    //should use LinkedList or something else instead
    for (i <- 1 until x.length) {
      arr(i) = arr(i - 1)
    }
    arr(0) = newValue
  }

  private def sum(arr: Array[Double]): Double = arr.reduceLeft(_ + _)

}