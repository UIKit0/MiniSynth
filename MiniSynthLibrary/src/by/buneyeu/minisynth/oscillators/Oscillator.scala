package by.buneyeu.minisynth.oscillators

import scala.math
import scala.math.Pi
import scala.math.sin
import by.buneyeu.minisynth.SampleRateDevice
import by.buneyeu.minisynth.SampleProcessor

class Oscillator(rate: Int) extends SampleRateDevice(rate) {
  val Tag = getClass.getSimpleName

  val mFrequency: MutableFrequency = new MutableFrequency(rate)
  var rads: Double = 0

  val twopiRate: Double = 2 * Pi / rate

  def processSample(sample: Double) : Double = processSample
  
  def processSample() : Double = {
    doSaw
  }

  def doWave(f: Double => Double) : Double = {
    rads += twopiRate * mFrequency.nextValue()
    rads %=  2 * Pi
    f(rads)
  }
  
  def doSaw() = doWave(saw)

  def setFreq = mFrequency.setFinalValue(_: Hz) 
  def setGlide = mFrequency.setGlide(_: Int)

  def saw(rads: Double): Double = {
    val phase = rads % (2 * Pi)

    if (phase <= Pi) {
      1 - phase * 2 / Pi
    } else {
      phase * 2 / Pi - 3
    }
  }

}