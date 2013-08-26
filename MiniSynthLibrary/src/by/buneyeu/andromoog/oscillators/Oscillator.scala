package by.buneyeu.andromoog.oscillators

import scala.math
import scala.math.Pi
import scala.math.sin

class Oscillator(rate: Double) {
  val Tag = getClass.getSimpleName

  val mFrequency: MutableFrequency = new MutableFrequency
  var rads: Double = 0

  val twopiRate: Double = 2 * Pi / rate

  def processSamples(buffer: Array[Double], freqHz: Double) = {
    System.out.println("freqHz = "+freqHz)
//    Log.d(Tag, "process samples " + freqHz)
    mFrequency.setFinalValue(freqHz)
    doSaw(buffer)
  }

  def setGlide = mFrequency.setGlide(_: Int)
  
  def doWave(buffer: Array[Double], f: Double => Double) = {
    for (i <- 0 to buffer.length - 1) {
      rads += twopiRate * mFrequency.nextValue()
      buffer(i) = f(rads)
    }
    rads %= 2 * Pi
  }

  def doSine = doWave(_: Array[Double], sin)

  def doSaw = doWave(_: Array[Double], saw)

  def saw(rads: Double): Double = {
    val phase = rads % (2 * Pi)

    if (phase <= Pi) {
      1 - phase * 2 / Pi
    } else {
      phase * 2 / Pi - 3
    }
  }

}