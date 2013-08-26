package by.buneyeu.andromoog.oscillators

import scala.math

class MutableFrequency {
  val Tag = getClass.getSimpleName

  type Hz = Double
  
  var mStep : Int = 0
  var mFinalValue : Hz = 0
  var mNumSteps : Int = 0
  var mStartValue : Hz = 0
  var mInc : Double = 0

  def setGlide(steps: Int) = mNumSteps = steps
  
  def setFinalValue(freq: Hz) = {
    mStartValue = getValue
    mFinalValue = freq
//    Log.d(Tag, "mFinalValue = "+mFinalValue)
    mInc = if (mNumSteps > 0) (mFinalValue - mStartValue) / mNumSteps else 0
    mStep = 0
  }
  
  def getValue: Hz = mStartValue + mInc * mStep  
  
  def nextValue(): Double = {
    val value = getValue
    mStep = mStep + 1 min mNumSteps
    value
  }

}