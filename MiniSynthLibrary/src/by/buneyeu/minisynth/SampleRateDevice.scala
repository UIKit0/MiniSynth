package by.buneyeu.minisynth

object SampleRateDevice {
  type Ms = Double
  
  val CFreq = 32.703196
  val Semitone = 1.05946309436

  val NoteToFreq = new Array[Double](100)

  NoteToFreq(0) = CFreq
  for (i <- 1 until NoteToFreq.length)
    NoteToFreq(i) = NoteToFreq(i - 1) * Semitone
}

abstract class SampleRateDevice(sampleRate: Int) {
  SampleRateDevice

  val NumSamples = sampleRate / 8 /* 1/8 second buffer */

  type Ms = Double
  type Hz = Double
  type HzPerMs = Double
  
  val MsInSec = 1000d
  
  def level(level1: Double, level2: Double, currentT: Ms, periodT: Ms): Double = {
    if (currentT > periodT)
      level2
    else {
      val s = level2 - level1
      val v = s / periodT
      level1 + v * currentT
    }
  }
}