package by.buneyeu.minisynth.oscillators

import scala.math
import by.buneyeu.minisynth.SampleRateDevice
import by.buneyeu.minisynth.SampleRateDevice._

class MutableFrequency(sampleRate: Int) extends SampleRateDevice(sampleRate) {
  var mStep : Int = 0
  var mFinalValue : Hz = 0
  var mNumSteps : Int = 0
  var mGlide : Ms = 0
  var mStartValue : Hz = 0
  var mInc : Hz = 0
  
  type Semitones = Double 
  var mPitch : Semitones = 0
  var mPitchMultiplicator: Double = 1
  
  def setGlide(glide: Ms) = mGlide = glide
  
  def setFinalValue(freq: Hz) = {
    mStartValue = getValue
    mFinalValue = freq
    val ms = MsInSec / sampleRate
    mNumSteps = (mGlide / ms).toInt
    mInc = if (mGlide > 0) (mFinalValue - mStartValue) / mNumSteps else 0
    mStep = 0
  }
  
  def setPitch(pitch: Double) = {
    mPitch = pitch
    mPitchMultiplicator = math.pow(SampleRateDevice.Semitone, mPitch)
  }

  def getValue: Hz = {
    val standardValue = if (mGlide > 0) mStartValue + mInc * mStep else mFinalValue
    val pitchedValue = standardValue * mPitchMultiplicator
    pitchedValue
  }
  
  def nextValue(): Double = {
    val value = getValue
    mStep = mStep + 1 min mNumSteps
    value
  }

}