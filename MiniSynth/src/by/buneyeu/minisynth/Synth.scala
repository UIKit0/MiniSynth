package by.buneyeu.minisynth

import by.buneyeu.andromoog.oscillators.Oscillator
import by.buneyeu.minisynth.MinimoogFilter
import javax.sound.sampled.SourceDataLine
import javax.sound.sampled.AudioFormat
import javax.sound.sampled.DataLine
import javax.sound.sampled.AudioSystem
import javax.sound.sampled.LineUnavailableException
import java.nio.ByteBuffer
import by.buneyeu.minisynth.loudness.LoudnessContour
import by.buneyeu.minisynth.NoteListener
import java.nio.file.Files
import java.nio.file.FileSystems
import java.nio.file.StandardOpenOption
import java.nio.charset.Charset
import java.io.BufferedWriter

class Synth(sampleRate: Int) extends SampleRateDevice(sampleRate) with NoteListener {

  val oscillator = new Oscillator(sampleRate)
  val minimoogFilter = new MinimoogFilter
  val loudnessCountur = new LoudnessContour(sampleRate)

  var mSoundThread: Thread = null
  var mNote = 1

  def noteOn(note: Integer, duration: Ms) = {
	  loudnessCountur.noteOn(note, duration)
	  
    m_stop = false;

    loudnessCountur.reset(30, 30, 7)
    mNote = note

    if (mSoundThread == null) {

      //      Log d (TAG, ""+note)
      mSoundThread = new Thread(new Runnable {
        def run() {
          generate(note)
        }
      })
      mSoundThread.start();
    }
  }

  def noteOff(note: Integer) = {
    loudnessCountur.noteOff(note)
  }

  var m_stop = false

  def generate(note: Int) = {
    // Open up audio output, using 44100hz sampling rate, 16 bit samples,
    // mono, and big
    // endian byte ordering
    val format = new AudioFormat(sampleRate.toFloat, 16, 1, true, true);
    val info = new DataLine.Info(classOf[SourceDataLine], format);

    if (!AudioSystem.isLineSupported(info)) {
      System.out.println("Line matching " + info + " is not supported.");
      throw new LineUnavailableException();
    }

    val line = AudioSystem.getLine(info).asInstanceOf[SourceDataLine]
    line.open(format, NumSamples * 2);
    line.start();


    val samples = new Array[Double](NumSamples)

    // Make our buffer size match audio system's buffer
    val buf = ByteBuffer.allocate(line.getBufferSize());

    oscillator.setGlide(50)
        val path = FileSystems.getDefault().getPath(".", "plot1.txt");
    val writer =  Files.newBufferedWriter( path, Charset.defaultCharset(), 
                                                  StandardOpenOption.CREATE);
    var t = 0d
    var maxt = 1000
    while (!m_stop && !Thread.currentThread.isInterrupted()) {
      val freqHz = SampleRateDevice.NoteToFreq(mNote)
    		  oscillator.setFreq(freqHz)
    		  
    		  def minimoogProcess = minimoogFilter.processSample(_: Double, 1000, 0.7)
      
      val processedSamples = samples map oscillator.processSample map loudnessCountur.processSample map minimoogProcess    
      	/* map 
      	loudnessCountur.processSample*/
      	
//      	processedSamples.foreach(System.out.println)
//        oscillator.processSamples(samples, freqHz) map loudnessCountur.processSamples(samples)   
        
//      minimoogFilter.processSamples(samples, 1000, 0.7)
//      loudnessCountur.processSamples(samples)
      if (t <= maxt) {
      printSamples(writer, processedSamples, t)
      t += NumSamples * MsInSec / sampleRate
      }
      
      buf.clear();
      for (i <- 0 until NumSamples)
        buf.putShort((Short.MaxValue * processedSamples(i)).toShort)

      line.write(buf.array(), 0, buf.position());
    }

    line.drain();
    line.close();
  }

  private val MsInSec = 1000

  def printSamples(writer: BufferedWriter, samples: Array[Double], startT: Ms) = {
    val builder = new StringBuilder()
    for (i <- 0 until samples.length) {
      val t = startT + i.toDouble * MsInSec / sampleRate
      builder ++= t.toString ++= " " ++= samples(i).toString ++= "\n"
    }
    val content = builder.toString()

    writer.write(content, 0, content.length)
  }
}
