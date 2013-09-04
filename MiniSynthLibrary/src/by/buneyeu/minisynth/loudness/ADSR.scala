package by.buneyeu.minisynth.loudness

import by.buneyeu.minisynth.NoteListener
import by.buneyeu.minisynth.SampleRateDevice

object ADSR {
  object State extends Enumeration {
    type State = Value
    val Attack, Decay, Sustain, Release = Value
  }

}

class ADSR(sampleRate: Int) extends SampleRateDevice(sampleRate) with NoteListener {
	ADSR
	
	import ADSR.State._
    
  var state = Attack
  
  var attack: Ms = 0
  var decay: Ms = 0
  var normalizedSustain : Double = 0

  var tFromLastState = 0d
  
  val MaxSustain = 10d
  
  //TODO separate reset method to setters
  def reset(attackIn: Ms, decayIn: Ms, sustainIn: Double /* 0-10 */ ) = {
    attack = attackIn
    decay = decayIn
    normalizedSustain = sustainIn / MaxSustain
  }

  var lastLevel = 0d

  def noteOn(note: Int) = ADSR.this.synchronized {
    updateStateTo(Attack)
    tFromLastState = lastLevel * attack
  }

  def noteOff(note: Int) = ADSR.this.synchronized {
    updateStateTo(Release)
  }

  def updateStateTo(stateIn: State) = ADSR.this.synchronized {
    tFromLastState = 0d
    state = stateIn
  }

  private def updateState() = ADSR.this.synchronized {
    if (state == Attack && tFromLastState > attack)
      updateStateTo(Decay)
    else if (state == Decay && tFromLastState > decay)
      updateStateTo(Sustain)
  }

  def update() : Double = {
    tFromLastState += MsInSec.toDouble / sampleRate
    updateState()
    lastLevel = updateLoudness
    lastLevel
  }
  
  private def updateLoudness =
    state match {
      case Attack => level(0, 1, tFromLastState, attack)
      case Decay => level(1, normalizedSustain, tFromLastState, decay)
      case Sustain => normalizedSustain
      case Release => level(normalizedSustain, 0, tFromLastState, decay)
    }
}