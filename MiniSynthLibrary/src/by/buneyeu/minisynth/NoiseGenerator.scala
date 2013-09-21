package by.buneyeu.minisynth

import scala.util.Random
import by.buneyeu.minisynth.NoiseType

object NoiseType extends Enumeration {
  type NoiseType = Value
  val White, Pink = Value
}

class NoiseGenerator(noiseType: NoiseType.Value) extends SampleGenerator {
  import NoiseType._

  class SimpleLPF extends SampleProcessor {
    var prevY = 0d

    def processSample(sample: Double): Double = {
      val a = 0.2d
      val y = a * sample + (1 - a) * prevY
      prevY = y
      y
    }
  }
  val lpf = new SimpleLPF

  @Override
  def processSample(): Double = {
    val value = Random.nextDouble * 2 - 1
    if (noiseType == White) {
      value
    } else {
      lpf.processSample(value)
    }
  }

}
