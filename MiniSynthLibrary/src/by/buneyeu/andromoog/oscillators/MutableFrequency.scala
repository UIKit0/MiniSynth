package by.buneyeu.andromoog.oscillators

import scala.math
import by.buneyeu.minisynth.SampleRateDevice

class MutableFrequency(sampleRate: Int) extends SampleRateDevice(sampleRate) {
  val Tag = getClass.getSimpleName

  type Ms = Double
  type Hz = Double
  type HzPerMs = Double
  
  var mStep : Int = 0
  var mFinalValue : Hz = 0
  var mNumSteps : Int = 0
  var mGlide : Ms = 0
  var mStartValue : Hz = 0
  var mInc : Hz = 0

  val MsInSec = 1000d
  
  def setGlide(glide: Ms) = mGlide = glide
  
  def setFinalValue(freq: Hz) = {
    mStartValue = getValue
    mFinalValue = freq
    val ms = MsInSec / sampleRate
    mInc = if (mGlide > 0) (mFinalValue - mStartValue) * ms / mGlide else 0
    mNumSteps = (mGlide / ms).toInt 
    mStep = 0
  }

  def getValue: Hz = if (mGlide > 0) mStartValue + mInc * mStep else mFinalValue  
  
  def nextValue(): Double = {
    val value = getValue
    mStep = mStep + 1 min mNumSteps
    value
  }

}