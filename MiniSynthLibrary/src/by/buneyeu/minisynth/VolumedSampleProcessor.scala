package by.buneyeu.minisynth

trait VolumedSampleProcessor extends SampleGenerator {

  private var normalizedVolume = 0d
  private val MaxVolume = 10

  /**
   * @param value should be between 0 and 10 like in original minimoog
   */
  def volume_=(value: Double): Unit = { //TODO reuse code of ADSR and this one
    require(value >= 0 && value <= MaxVolume, "Volume should be in range between 0 and " + MaxVolume + "!")
    normalizedVolume = value / MaxVolume
  }

  def volume = normalizedVolume * MaxVolume

  abstract override def processSample(sample: Double): Double = {
    val supered = super.processSample(sample)
    supered * normalizedVolume
  }
  abstract override def processSample(): Double = {
    val supered = super.processSample()
    supered * normalizedVolume
  }
}