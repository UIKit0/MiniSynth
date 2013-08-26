package by.buneyeu.minisynth

import scala.math._

class MinimoogFilter {
  val Tag = getClass.getSimpleName

  val rate = 44100.0
  val nyquist = rate / 2

  type Hz = Double

  val y = Array[Double](0, 0, 0, 0)
  val oldy = Array[Double](0, 0, 0)

    var y1, y2, y3, y4, oldy1, oldy2, oldy3 = 0d
    var oldx = 0d
    
  def processSamples(buffer: Array[Double], cutoffIn: Hz, resIn: Double) = {
    //Init

    val f = 2 * cutoffIn / rate; //[0 - 1]
    val p = f * (1.8f - 0.8f * f);
    val k = p + p - 1.f;

    val t = (1.f - p) * 1.386249f;
    val t2 = 12.f + t * t;
    val r = resIn * (t2 + 6.f * t) / (t2 - 6.f * t);

    def processSample(input: Double): Double = {
      // process input
      val x = input - r * y4;

      //Four cascaded onepole filters (bilinear transform)
      y1 = x * p + oldx * p - k * y1;
      y2 = y1 * p + oldy1 * p - k * y2;
      y3 = y2 * p + oldy2 * p - k * y3;
      y4 = y3 * p + oldy3 * p - k * y4;

      //Clipper band limited sigmoid
      y4 -= (y4 * y4 * y4) / 6.f;

      oldx = x; oldy1 = y1; oldy2 = y2; oldy3 = y3;
      y4
    }
    for (i <- 0 until buffer.length) {
      buffer(i) = processSample(buffer(i))
    }

  }

}