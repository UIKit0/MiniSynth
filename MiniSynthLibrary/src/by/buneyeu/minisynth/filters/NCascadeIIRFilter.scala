package by.buneyeu.minisynth.filters

import by.buneyeu.minisynth.SampleProcessor

class NCascadeIIRFilter(a: Array[Double], b: Array[Double], n: Int) extends SampleProcessor {

  val filters = Array.fill(n)(new IIRFilter(a, b))

  def processSample(value: Double, index: Int): Double = {
    filters(index).processSample(
      if (index == 0) {
        value
      } else {
        processSample(value, index - 1)
      })
  }

  def processSample(value: Double): Double = {
    processSample(value, filters.length - 1);
  }
}