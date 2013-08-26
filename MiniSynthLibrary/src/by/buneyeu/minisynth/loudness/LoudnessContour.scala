package by.buneyeu.minisynth.loudness

import by.buneyeu.minisynth.SampleRateDevice
import by.buneyeu.minisynth.NoteListener

class LoudnessContour(sampleRate: Int) extends SampleRateDevice(sampleRate) with NoteListener {
  val Tag = getClass getSimpleName

  var attackMs = 0d
  var decay = 0d
  var endValue = 0d

  def reset(attackMsIn: Double /* 0 - 100 ms */ , decayIn: Double /* */ , sustainIn: Double /* 0-10 */ ) = {
    attackMs = attackMsIn
    decay = decayIn
    endValue = sustainIn / MaxSustain
    startMs = 0

  }

  val MaxSustain = 10;

  var startMs = 0d
  def noteOn(note: Integer, duration: Ms) = {
    startMs = 0
  }

  def noteOff(note: Integer) = {}


  def processSamples(buffer: Array[Double]) = {
    val attackMsHere = attackMs
    val decayHere = decay
    val startedFromZero = startMs == 0
    for (i <- 0 until buffer.length) {

      startMs += 1000d / sampleRate
      val loudness = getLoudnessFromStart(startMs)
      buffer(i) *= loudness
    }
  }

  def getLoudnessFromStart(start: Ms): Double /* 0 - 1 */ = {
    val startFromAttack = start - attackMs

    if (start <= attackMs) {
      start / attackMs
    } else if (startFromAttack <= decay) {
      1 - startFromAttack / decay + endValue * startFromAttack / decay
    } else
      endValue
  }
}