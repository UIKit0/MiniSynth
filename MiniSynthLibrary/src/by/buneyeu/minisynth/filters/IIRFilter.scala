package by.buneyeu.minisynth.filters

import by.buneyeu.minisynth.SampleProcessor
import by.buneyeu.minisynth.SampleRateDevice

//TODO check it carefully and refactor hard in scala way
class IIRFilter(sampleRate: Int, a: Array[Double], b: Array[Double]) extends SampleRateDevice(sampleRate) with SampleProcessor {
  require(b.length > 0, "To create IIR filter you should define at least one b coeff!")

  private val x = Array.fill(b.length)(0.0)
  private val y = Array.fill(a.length)(0.0)
  
  def processSample(xIn: Double) : Double = {
    delay(x, xIn)
    val yOut = sum(mult(b, x)) + sum(mult(a, y))
    delay(y, yOut)
    yOut
  }

  def delay(arr: Array[Double], newValue: Double) = {
    //very stupid delay implementation with O(n) complexity,
    //should use LinkedList or something else instead
    for (i <- 1 until x.length) {
      arr(i) = arr(i - 1)
    }
    arr(0) = newValue
  }
  
  def sum(arr: Array[Double]) : Double = arr.reduceLeft(_ + _)
  def mult(arr1: Array[Double], arr2: Array[Double]) : Array[Double] = {
    require(arr1.length == arr2.length, "Arrays should have the same length!")
    val multArr = Array.fill(arr1.length)(0.0)
    for(i <- 0 until arr1.length){
      multArr(i) = arr1(i) * arr2(i)
    }
    multArr
  }
}