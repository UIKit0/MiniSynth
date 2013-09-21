package by.buneyeu.minisynth.oscillators

import scala.math
import scala.math.Pi
import scala.math.sin
import by.buneyeu.minisynth.SampleRateDevice
import by.buneyeu.minisynth.SampleRateDevice._
import by.buneyeu.minisynth.SampleProcessor
import by.buneyeu.minisynth.SampleGenerator

object Oscillator {
  val Tag = getClass.getSimpleName

  object Waveform extends Enumeration {
    type State = Value
    val Triangle, AscendingSawtooth, DescendingSawtooth, SawtoothTriangle, Square, WideRectangular, NarrowRectangular = Value
  }

}

class Oscillator(sampleRate: Int) extends SampleRateDevice(sampleRate) with SampleGenerator {
  Oscillator

  import Oscillator.Waveform._
  
  var waveform = Triangle
  
  private val mFrequency: MutableFrequency = new MutableFrequency(sampleRate)
  private var mRads: Double = 0

  @Override
  def processSample(): Double = {
    doWave(
      waveform match {
        case Triangle => triangle
        case AscendingSawtooth => ascendingSawtooth
        case DescendingSawtooth => descendingSawtooth
        case SawtoothTriangle => sawtoothTriangle
        case Square => square
        case WideRectangular => wideRectangular
        case NarrowRectangular => narrowRectangular
      })
  }

  private def doWave(f: Double => Double): Double = {
    mRads += 2 * Pi * mFrequency.nextValue() / sampleRate
    val newRads = mRads % (2 * Pi)
    if (newRads != mRads && synchronizedOscillator.isDefined) {
      synchronizedOscillator.get.mRads = newRads
    } 
    mRads = newRads
    f(mRads)
  }
  
  def resetPhase() = mRads = 0
  
  var synchronizedOscillator : Option[Oscillator] = None

  def pitch_=(pitch: Semitones) = mFrequency.pitch = pitch
  def pitch = mFrequency.pitch

  def freq_= (freq: Hz) = mFrequency.finalValue = freq
  def freq = mFrequency.finalValue
  
  def glide_= (glide: Ms) = mFrequency.glide = glide
  def glide = mFrequency.glide

  def triangle(phase: Double): Double = sawtoothTriangle(phase, 0)

  def ascendingSawtooth(phase: Double): Double = {
    //TODO prevent too sharp decline here
    phase / Pi - 1
  }

  def descendingSawtooth(phase: Double) = ascendingSawtooth(phase) * -1
  
  def sawtoothTriangle(phase: Double) : Double = sawtoothTriangle(phase, 0.1)
  
  /* sawtoothLevel = 1 => sawtooth-triangle wave == wave with sawtooth impulses
   * sawtoothLevel = 0 => sawtooth-triangle wave == triangle wave 
   */
  private def sawtoothTriangle(phase: Double, sawtoothLevel: Double): Double = {
    if (phase <= Pi) {
      level(1 - sawtoothLevel * 2, -1, phase, Pi)
    } else {
      level(-1, 1, phase - Pi, Pi)
    }
  }
  
  private val wideRectangularDutyCycle = 35
  def wideRectangular(phase: Double) = pulse(phase, wideRectangularDutyCycle)
  
  private val narrowRectangularDutyCycle = 15
  def narrowRectangular(phase: Double) = pulse(phase, narrowRectangularDutyCycle)
  
  private val squareDutyCycle = 50
  def square(phase: Double) = pulse(phase, squareDutyCycle)
  
  type Percent = Double
  val Percents = 100
  
  private def pulse(phase: Double, dutyCycle: Percent) : Double = {
    //TODO prevent too sharp decline here
    if (phase <= 2 * Pi * dutyCycle / Percents)
      1
    else
      -1
  }
  
}