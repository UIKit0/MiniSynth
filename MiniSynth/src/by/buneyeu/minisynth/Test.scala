package by.buneyeu.minisynth

import java.nio.file.FileSystems
import java.nio.file.Files
import java.nio.charset.Charset
import java.nio.file.StandardOpenOption
import scala.collection.mutable.ListBuffer
import by.buneyeu.minisynth.oscillators.MutableFrequency
import by.buneyeu.minisynth.loudness.LoudnessContour
import by.buneyeu.minisynth.oscillators.Oscillator

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
  
  
  def plotLoudness(attack: Ms, decay: Ms, normalizedSustain: Double) {
    val sampleRate = 44100
    val inc: Ms = MsInSec.toDouble / sampleRate
    val timeToPlot : Ms = 1000
    val steps: Int = (timeToPlot / inc).toInt

    val loudnesses = new Array[Double](steps)
    val valuesToPlot = new Array[(Double, Double)](steps)
    
    val countour = new LoudnessContour(sampleRate)
    countour.reset(100, 100, 7)

    def thread[F](f: => F) = (new Thread(new Runnable() { def run() { f } })).start

    thread({
      countour.noteOn(1)
      Thread.sleep(250)
      countour.noteOff(1)
      Thread.sleep(30)
      countour.noteOn(1)
      Thread.sleep(250)
      countour.noteOff(1)
      Thread.sleep(30)
      countour.noteOn(1)
      Thread.sleep(250)
      countour.noteOff(1)
      System.out.println("end");
    })

    for (
      i <- 0 until steps;
      t = inc * i
    ) {
      loudnesses(i) = countour.processSample(1)
      valuesToPlot(i) = (t, loudnesses(i))
    }
    System.out.println("end1");
    plot("loudness.txt", valuesToPlot)
    System.out.println("end2");
  }
}