package by.buneyeu.minisynth

import scala.math._
import by.buneyeu.minisynth.loudness.ADSR

class MinimoogFilter(sampleRate: Int, private var _cutOff: Double, private var _res: Double) extends SampleRateDevice(sampleRate) with SampleProcessor {
  val Tag = getClass.getSimpleName

  val nyquist = sampleRate / 2

  val y = Array[Double](0, 0, 0, 0)
  val oldy = Array[Double](0, 0, 0)

  val adsr = new ADSR(sampleRate)

  //TODO separate reset method to setters
  def reset = adsr.reset(_: Ms, _: Ms, _: Double)
  
  var oldx = 0d

  def nextCutOff(): Double = {
    val level = adsr.update()
//    level * _cutOff
    _cutOff
  }
  
  def cutOff_= (value: Double) = {
    _cutOff = value
  }
  
  def res_= (value:Double) = {
    require(_res >= 0 && _res <= 1, "Res should be in range [0..1]")
    _res = value
  } 

  override def processSample(input: Double) : Double = {
    val actualCutOff = nextCutOff()
    val f = 2 * actualCutOff / sampleRate //[0 - 1]
    val p = f * (1.8f - 0.8f * f)
    val k = p + p - 1.f

    val t = (1.f - p) * 1.386249f
    val t2 = 12.f + t * t
    val r = _res * (t2 + 6.f * t) / (t2 - 6.f * t)
  
    // process input
    val x = input - r * y(3)

    //Four cascaded onepole filters (bilinear transform)
    y(0) = x * p + oldx * p - k * y(0)
    y(1) = y(0) * p + oldy(0) * p - k * y(1)
    y(2) = y(1) * p + oldy(1) * p - k * y(2)
    y(3) = y(2) * p + oldy(2) * p - k * y(3)

    //Clipper band limited sigmoid
    y(3) -= pow(y(3), 3) / 6.f

    oldx = x
    oldy(0) = y(0)
    oldy(1) = y(1)
    oldy(2) = y(2)
    
    y(3)
  }
}