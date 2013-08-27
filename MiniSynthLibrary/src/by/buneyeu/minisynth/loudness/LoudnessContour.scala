package by.buneyeu.minisynth.loudness

import by.buneyeu.minisynth.SampleRateDevice
import by.buneyeu.minisynth.NoteListener

object LoudnessContour {
  val Tag = getClass getSimpleName

  type Ms = Double

  def getLoudness(t: Ms, attack: Ms, decay: Ms, normalizedSustain: Double): Double /* 0 - 1 */ = {
    val startFromAttack = t - attack

    if (t <= attack) {
      t / attack
    } else if (startFromAttack <= decay) {
      (normalizedSustain - 1) * startFromAttack / decay + 1
    } else
      normalizedSustain
  }
  
}

class LoudnessContour(sampleRate: Int) extends SampleRateDevice(sampleRate) with NoteListener {
  LoudnessContour

  var attack: Ms = 0d
  var decay: Ms = 0d
  var normalizedSustain = 0d

  val MaxSustain = 10;
  
  def reset(attackIn: Ms, decayIn: Ms, sustainIn: Double /* 0-10 */ ) = {
    attack = attackIn
    decay = decayIn
    normalizedSustain = sustainIn / MaxSustain
    startMs = 0
  }


  var startMs = 0d
  
  def noteOn(note: Integer, duration: Ms) = {
    startMs = 0d
  }

  def noteOff(note: Integer) = { startMs = 0d }

  private val MsInSec = 1000

  def processSample(sample: Double): Double = {
    startMs += MsInSec.toDouble / sampleRate
    val loudness = LoudnessContour.getLoudness(startMs, attack, decay, normalizedSustain)
    sample * loudness
  }

  def processSamples(buffer: Array[Double]) = {
    for (i <- 0 until buffer.length) {
      buffer(i) = processSample(buffer(i))
    }
  }

}