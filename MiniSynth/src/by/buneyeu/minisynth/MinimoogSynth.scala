package by.buneyeu.minisynth

import by.buneyeu.minisynth.oscillators.Oscillator
import by.buneyeu.minisynth.loudness.ADSR
import javax.sound.sampled.AudioFormat
import javax.sound.sampled.DataLine
import javax.sound.sampled.SourceDataLine
import javax.sound.sampled.LineUnavailableException
import javax.sound.sampled.AudioSystem
import java.nio.ByteBuffer
import by.buneyeu.minisynth.loudness.LoudnessContour
import by.buneyeu.minisynth.Implicits._
import by.buneyeu.minisynth.oscillators.VolumedOscillator

class MinimoogSynth(sampleRate: Int) extends SampleRateDevice(sampleRate) with NoteListener  {
  import NoiseType._
  
  val NumOfOscs = 3
  val oscillators = Array.fill(NumOfOscs)(new VolumedOscillator(sampleRate))
  private val whiteNoiseGenerator = new NoiseGenerator(White)
  private val pinkNoiseGenerator = new NoiseGenerator(Pink)
  private val loudnessContour = new LoudnessContour(sampleRate)

  private val noteListeners : Array[NoteListener] = Array(loudnessContour) 
  
  def thread[F](f: => F) = (new Thread( new Runnable() { def run() { f } } )).start

  thread { 
       // Open up audio output, using 44100hz sampling rate, 16 bit samples,
    // mono, and big
    // endian byte ordering
    val format = new AudioFormat(sampleRate.toFloat, 16, 1, true, true)
    val info = new DataLine.Info(classOf[SourceDataLine], format)

    if (!AudioSystem.isLineSupported(info)) {
      System.out.println("Line matching " + info + " is not supported.")
      throw new LineUnavailableException()
    }

    val line = AudioSystem.getLine(info).asInstanceOf[SourceDataLine]
    line.open(format, NumSamples * 2)
    line.start()

    // Make our buffer size match audio system's buffer
    val buf = ByteBuffer.allocate(line.getBufferSize())

    while (!Thread.currentThread.isInterrupted) {
      val freqHz = SampleRateDevice.NoteToFreq(mNote)
      
      oscillators.foreach(_.freq = freqHz)
      
      val soundGenerators = oscillators // :+ pinkNoiseGenerator

      val processedSamples = Array.fill(NumSamples)({
        val soundSamples = soundGenerators.map(_.processSample)
        val sumSample = soundSamples.reduce(_ + _)
        loudnessContour.processSample(sumSample) * 0.2
      })
      
      processedSamples.filter(_ >= 1.0).foreach(
        pickedValue => System.out.println("peak!" + pickedValue))
      
      
      buf.clear()
      processedSamples
        .map(Short.MaxValue * _ toShort)
        .foreach(buf.putShort)

      line.write(buf.array, 0, buf.position)
    }

    line.drain()
    line.close()
  }
  
  private var mNote : Int = 60
  
  def noteOn(note: Int) = {
    noteListeners.foreach(_.noteOn(note))
    
    mNote = note
  }

  def noteOff(note: Int) = {
    noteListeners.foreach(_.noteOff(note))
  }
  
}