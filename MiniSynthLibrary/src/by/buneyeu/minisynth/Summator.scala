package by.buneyeu.minisynth

import scala.collection.immutable.Map

class Summator(private val processors: SampleProcessor*) extends SampleProcessor {
  def processSample(sample: Double): Double = {
    processors.map(_.processSample(sample)).reduceLeft(_ + _) / processors.length
  }
}