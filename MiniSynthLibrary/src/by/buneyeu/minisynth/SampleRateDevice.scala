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

class SampleRateDevice(sampleRate: Int) {
  SampleRateDevice

  val NumSamples = sampleRate / 8 /* 1/8 second buffer */

}