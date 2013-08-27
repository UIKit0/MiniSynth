package by.buneyeu.minisynth

import java.nio.file.FileSystems
import java.nio.file.Files
import java.nio.charset.Charset
import java.nio.file.StandardOpenOption
import by.buneyeu.andromoog.oscillators.MutableFrequency
import scala.collection.mutable.ListBuffer
import by.buneyeu.andromoog.oscillators.Oscillator
import by.buneyeu.minisynth.loudness.LoudnessContour

object Test {

  type Ms = Double
  type Hz = Double 
  
  def plotFreq(sampleRate: Int, glide: Ms, startValue: Hz, endValue: Hz, changeNoteAfter: Ms) {
    val afterEnd: Ms = 500
    
    val inc: Ms = 0.1
    val plotTime = changeNoteAfter + glide + afterEnd
    val steps: Int = (plotTime / inc).toInt
    var currentMs = 0d
    
    val freq = new MutableFrequency(sampleRate)
    freq.setGlide(glide)
    freq.setFinalValue(endValue)
    
    val freqs = new ListBuffer[Double]
    var set = false

    val builder = new StringBuilder
    
    while (currentMs <= plotTime) {
      freqs += freq.nextValue()
//      if (currentMs >= changeNoteAfter && !set) {
//        freq.setFinalValue(endValue)
//        set = true
//      }
//      
      builder ++= currentMs.toString ++= " " ++= freqs.last.toString ++= "\n"
      currentMs += inc
    }
    
    val path = FileSystems.getDefault().getPath(".", "mutable_frequency.txt");
    Files.write(path, builder.toString.getBytes, StandardOpenOption.CREATE)
    
  }

  val MsInSec = 1000
  
  
  def plot(filename: String, values: Array[(Double, Double)]) = {
    val builder = new StringBuilder
    values.foreach(tuple => {
      val ms = tuple._1
      val value = tuple._2
      builder ++= ms.toString ++= " " ++= value.toString ++= "\n"
    })
    val path = FileSystems.getDefault().getPath(".", filename);
    Files.write(path, builder.toString.getBytes, StandardOpenOption.CREATE)
  }

  def plotOscillator(sampleRate: Int, freq: Hz, plotTime: Ms) {
    val osc = new Oscillator(sampleRate)
    osc.setGlide(0)
    osc.setFreq(freq)//TODO
    
    val length = (sampleRate * plotTime / MsInSec).toInt
    val valuesToPlot: Array[(Double, Double)] = new Array[(Double, Double)](length)
    for (
      i <- 0 until length;
      val ms = i.toDouble * MsInSec / sampleRate;
      val value = osc.processSample
    ) valuesToPlot(i) = (ms, value)
    
    
    plot("oscillator.txt", valuesToPlot)
  } 
  
  
  def plotLoudness(attack: Ms, decay: Ms, normalizedSustain: Double, start: Ms, end: Ms) {
    val inc: Ms = 0.1
    val steps: Int = ((end - start) / inc).toInt
    val loudnesses = new Array[Double](steps)
    val valuesToPlot: Array[(Double, Double)] = new Array[(Double, Double)](steps)
    for (i <- 0 until steps; 
    val t = start + inc * i) {
      
      loudnesses(i) = LoudnessContour.getLoudness(t, attack, decay, normalizedSustain)
      
      valuesToPlot(i) = (t, loudnesses(i))
    }
    
    plot("loudness.txt", valuesToPlot)
  }
}