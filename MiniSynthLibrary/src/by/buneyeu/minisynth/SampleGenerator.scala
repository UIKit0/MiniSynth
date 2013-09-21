package by.buneyeu.minisynth

trait SampleGenerator {
  def processSample() : Double
  def processSamples(numSamples: Int) : Array[Double] = Array.fill[Double](numSamples)(processSample)
}
