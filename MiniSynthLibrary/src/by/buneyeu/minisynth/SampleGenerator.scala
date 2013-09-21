package by.buneyeu.minisynth

trait SampleGenerator extends SampleProcessor {
  
  def processSample(value: Double) : Double = processSample()
  
  def processSample() : Double
  def processSamples(numSamples: Int) : Array[Double] = Array.fill[Double](numSamples)(processSample)
}
