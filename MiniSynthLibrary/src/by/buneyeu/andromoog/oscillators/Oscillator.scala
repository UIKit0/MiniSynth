package by.buneyeu.andromoog.oscillators

import scala.math
import scala.math.Pi
import scala.math.sin
import by.buneyeu.minisynth.SampleRateDevice

class Oscillator(rate: Int) extends SampleRateDevice(rate) {
  val Tag = getClass.getSimpleName

  val mFrequency: MutableFrequency = new MutableFrequency(rate)
  var rads: Double = 0

  val twopiRate: Double = 2 * Pi / rate

  def processSamples(buffer: Array[Double]) : Array[Double] = {
//    Log.d(Tag, "process samples " + freqHz)
    
    doSaw(buffer)
    buffer
  }

  def processSample() : Double = {
    doSaw
  }

  def processSample(value: Double) : Double = {
    doSaw
  }
  def doWave(f: Double => Double) : Double = {
    rads += twopiRate * mFrequency.nextValue()
    val v = f(rads)
    rads %= 2 * Pi
    v
  }
  
  def doSaw() = doWave(saw)

  type Hz = Double
  def setFreq = mFrequency.setFinalValue(_: Hz) 
  def setGlide = mFrequency.setGlide(_: Int)
  
  def doWave(buffer: Array[Double], f: Double => Double) = {
    for (i <- 0 to buffer.length - 1) {
      rads += twopiRate * mFrequency.nextValue()
      buffer(i) = f(rads)
    }
    rads %= 2 * Pi
  }

  def doSine = doWave(_: Array[Double], sin)

  def doSaw(buffer: Array[Double]) = doWave(buffer, saw)

  def saw(rads: Double): Double = {
    val phase = rads % (2 * Pi)

    if (phase <= Pi) {
      1 - phase * 2 / Pi
    } else {
      phase * 2 / Pi - 3
    }
  }

}