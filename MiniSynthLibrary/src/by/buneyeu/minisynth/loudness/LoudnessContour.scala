package by.buneyeu.minisynth.loudness

import by.buneyeu.minisynth.SampleRateDevice
import by.buneyeu.minisynth.NoteListener
import by.buneyeu.minisynth.SampleProcessor

object LoudnessContour {
  val Tag = getClass getSimpleName
}

class LoudnessContour(sampleRate: Int) extends ADSR(sampleRate) with SampleProcessor {
  import State._

  override def processSample(sample: Double): Double = {
    val loudness = update();
    sample * loudness
  }
}