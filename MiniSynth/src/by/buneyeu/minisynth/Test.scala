package by.buneyeu.minisynth

import java.nio.file.FileSystems
import java.nio.file.Files
import java.nio.charset.Charset
import java.nio.file.StandardOpenOption
import scala.collection.mutable.ListBuffer
import by.buneyeu.minisynth.oscillators.MutableFrequency
import by.buneyeu.minisynth.oscillators.Oscillator
import by.buneyeu.minisynth.loudness.LoudnessContour

object Test {

  type Ms = Double
  type Hz = Double
  private val MsInSec = 1000
  
  def plotFreq(sampleRate: Int, glide: Ms, startValue: Hz, endValue: Hz, changeNoteAfter: Ms) {
    val afterEnd: Ms = 500

    val inc: Ms = 0.1
    val plotTime = changeNoteAfter + glide + afterEnd
    val steps: Int = (plotTime / inc).toInt
    var currentMs = 0d

    val freq = new MutableFrequency(sampleRate)
    freq.glide = glide
    freq.finalValue = endValue

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

  def writeStringToFile(filename: String, stringToWrite: String) : Unit = {
    val path = FileSystems.getDefault().getPath(".", filename);
    Files.write(path, stringToWrite.getBytes, StandardOpenOption.CREATE)
  }
  
  def plot(filename: String, values: Array[Double]) =
    plot[Double](filename, values, (builder, value) => {
      builder ++= value.toString ++= "\n"
    })

  def plot(filename: String, values: Array[(Ms, Double)]) =
    plot[(Double, Double)](filename, values, (builder, tuple) => {
      val ms = tuple._1
      val value = tuple._2
      builder ++= ms.toString ++= " " ++= value.toString ++= "\n"
    })
  
  def plot[T](filename: String, values: Array[T], writeValueToBuilder: (StringBuilder, T) => Unit) : Unit = {
    val builder = new StringBuilder
    values.foreach(value => writeValueToBuilder(builder, value))
    writeStringToFile(filename, builder.toString)
  }

  private def plotOscillatorWithTime(sampleRate: Int, freq: Hz, plotTime: Ms, waveform: Oscillator.Waveform.Value) = {
    val osc = new Oscillator(sampleRate)
    osc.glide = 0
    osc.freq = freq //TODO
    osc.waveform = waveform

    val length = (sampleRate * plotTime / MsInSec).toInt
    val valuesToPlot = new Array[(Ms, Double)](length)
    for (
      i <- 0 until length;
      val ms = i.toDouble * MsInSec / sampleRate;
      val value = osc.processSample
    ) valuesToPlot(i) = (ms, value)

    plot("plots/" + waveform.toString() + ".txt", valuesToPlot)
  }

  private def plotOscillatorWithoutTime(sampleRate: Int, freq: Hz, plotTime: Ms, waveform: Oscillator.Waveform.Value) = {
    val osc = new Oscillator(sampleRate)
    osc.glide = 0
    osc.freq = freq //TODO
    osc.waveform = waveform

    val length = (sampleRate * plotTime / MsInSec).toInt
    val valuesToPlot  = new Array[Double](length)
    for (
      i <- 0 until length
    ) valuesToPlot(i) = osc.processSample

    plot("plots/" + waveform.toString() + ".txt", valuesToPlot)
  }

  def plotOscillatorsWithTime(sampleRate: Int, freq: Hz, plotTime: Ms) =
    Oscillator.Waveform.values.foreach(plotOscillatorWithTime(sampleRate, freq, plotTime, _))

  def plotOscillatorsWithoutTime(sampleRate: Int, freq: Hz, plotTime: Ms) =
    Oscillator.Waveform.values.foreach(plotOscillatorWithoutTime(sampleRate, freq, plotTime, _))
    
  def plotLoudness(attack: Ms, decay: Ms, normalizedSustain: Double) {
    val sampleRate = 44100
    val inc: Ms = MsInSec.toDouble / sampleRate
    val timeToPlot: Ms = 1000
    val steps: Int = (timeToPlot / inc).toInt

    val loudnesses = new Array[Double](steps)
    val valuesToPlot = new Array[(Double, Double)](steps)

    val countour = new LoudnessContour(sampleRate)
    countour.attack = 100
    countour.decay = 100
    countour.sustain = 7

    countour.noteOn(1)
    for (
      i <- 0 until steps;
      t = inc * i
    ) {
      loudnesses(i) = countour.processSample(1)
      valuesToPlot(i) = (t, loudnesses(i))
      
      if (i == 250d / inc) {
        countour.noteOff(1)
      } else if (i == 280d / inc) {
        countour.noteOn(1)
      } else if (i == (280d + 250d)/inc) {
        countour.noteOn(1)
      } else if (i == (280d + 250d + 50d)/inc) {
        countour.noteOn(1)
      } else if (i == (280d + 250d + 50d + 250d)/inc) {
        countour.noteOff(1)
      }
    }
    plot("loudness.txt", valuesToPlot)
  }
}