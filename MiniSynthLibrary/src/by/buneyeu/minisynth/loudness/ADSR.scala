package by.buneyeu.minisynth.loudness

import by.buneyeu.minisynth.NoteListener
import by.buneyeu.minisynth.SampleRateDevice
import by.buneyeu.minisynth.SampleRateDevice._

object ADSR {
  object State extends Enumeration {
    type State = Value
    val Attack, Decay, Sustain, Release = Value
  }

}

class ADSR(sampleRate: Int) extends SampleRateDevice(sampleRate) with NoteListener {
  ADSR
  
  import ADSR.State._
  
  private var state = Attack
  
  var attack: Ms = 0
  var decay: Ms = 0
  private var normalizedSustain : Double = 1

  private var _sustain = 1d

  /**
   * @param value should be between 0 and 10 like in original minimoog
   */
  def sustain_=(value: Double): Unit = {
    require(value >= 0 && value <= 10, "Sustain should be in range between 0 and 10!")
    normalizedSustain = value / MaxSustain
  }
  
  def sustain = normalizedSustain * MaxSustain
  
  private var tFromLastState = 0d
  
  private val MaxSustain = 10d
  
  private var lastLevel = 0d

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