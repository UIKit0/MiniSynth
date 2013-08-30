package by.buneyeu.minisynth.loudness

import by.buneyeu.minisynth.SampleRateDevice
import by.buneyeu.minisynth.NoteListener
import by.buneyeu.minisynth.SampleProcessor

object LoudnessContour {
  val Tag = getClass getSimpleName
}

object State extends Enumeration {
  type State = Value
  val Attack, Decay, Sustain, Release = Value
}

class LoudnessContour(sampleRate: Int) extends SampleRateDevice(sampleRate) with NoteListener with SampleProcessor {
  LoudnessContour

  import State._
    
  var state = Attack
  
  var attack: Ms = 0
  var decay: Ms = 0
  var normalizedSustain : Double = 0

  var tFromLastState = 0d
  
  val MaxSustain = 10d
  
  def reset(attackIn: Ms, decayIn: Ms, sustainIn: Double /* 0-10 */ ) = {
    attack = attackIn
    decay = decayIn
    normalizedSustain = sustainIn / MaxSustain
  }

  var lastLoudness = 0d

  def noteOn(note: Int) = this.synchronized {
    updateStateTo(Attack)
    tFromLastState = lastLoudness * attack
  }

  def noteOff(note: Int) = this.synchronized {
    updateStateTo(Release)
  }

  def updateStateTo(stateIn: State) = this.synchronized {
    tFromLastState = 0d
    state = stateIn
  }

  def updateState() = this.synchronized {
    if (state == Attack && tFromLastState > attack)
      updateStateTo(Decay)
    else if (state == Decay && tFromLastState > decay)
      updateStateTo(Sustain)
  }

  override def processSample(sample: Double): Double = {
    tFromLastState += MsInSec.toDouble / sampleRate
    updateState()
    lastLoudness = updateLoudness
    sample * lastLoudness
  }

  def updateLoudness =
    state match {
      case Attack => level(0, 1, tFromLastState, attack)
      case Decay => level(1, normalizedSustain, tFromLastState, decay)
      case Sustain => normalizedSustain
      case Release => level(normalizedSustain, 0, tFromLastState, decay)
    }
}