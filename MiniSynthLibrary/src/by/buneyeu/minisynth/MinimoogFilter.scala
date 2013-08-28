package by.buneyeu.minisynth

import scala.math._

class MinimoogFilter(sampleRate: Int, var cutOff: Double, var res: Double) extends SampleRateDevice(sampleRate) with SampleProcessor {
  val Tag = getClass.getSimpleName

  val nyquist = sampleRate / 2

  val y = Array[Double](0, 0, 0, 0)
  val oldy = Array[Double](0, 0, 0)

  var y1, y2, y3, y4, oldy1, oldy2, oldy3 = 0d
  var oldx = 0d

  def setCutOff = cutOff = _: Hz
  def setRes = res = _: Double
  
  //TODO optimize defs to vars
  private def f = 2 * cutOff  / sampleRate //[0 - 1]
  private def p = f * (1.8f - 0.8f * f)
  private def k = p + p - 1.f
  
  private def t = (1.f - p) * 1.386249f
  private def t2 = 12.f + t * t
  private def r = res * (t2 + 6.f * t) / (t2 - 6.f * t)

  
  override def processSample(input: Double) : Double = {
    // process input
    val x = input - r * y4

    //Four cascaded onepole filters (bilinear transform)
    y(0) = x * p + oldx * p - k * y(0)
    y(1) = y(0) * p + oldy1 * p - k * y(1)
    y(2) = y(1) * p + oldy2 * p - k * y(2)
    y(3) = y(2) * p + oldy3 * p - k * y(3)

    //Clipper band limited sigmoid
    y(3) -= pow(y(3), 3) / 6.f

    oldx = x
    oldy1 = y1 
    oldy2 = y2 
    oldy3 = y3
    
    y4
  }
}