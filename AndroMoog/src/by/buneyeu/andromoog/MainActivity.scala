package by.buneyeu.andromoog

import android.app.Activity
import android.os.Bundle
import android.util.Log
import com.google.synthesizer.android.widgets.piano.PianoView
import com.google.synthesizer.core.midi.MidiListener
import android.media.AudioTrack
import android.media.AudioManager
import android.media.AudioFormat
import scala.util.Random

class MainActivity extends Activity {
  val TAG = getClass.getSimpleName

  var mPiano: PianoView = null

  var m_stop = false
  val sampleRate = 44100
  val mNumSamples = sampleRate / 8 /* 1/8 second buffer */
  val mAudioTrack: AudioTrack = new AudioTrack(AudioManager.STREAM_MUSIC, sampleRate, AudioFormat.CHANNEL_OUT_MONO,
    AudioFormat.ENCODING_PCM_16BIT, mNumSamples,
    AudioTrack.MODE_STREAM)
  
  var mSoundThread: Thread = null
  
  val C_FREQ = 32.703
  val SEMITONE = 1.05946309436

  def valuesToSamples(values: Array[Double], data: Array[Byte]) = {
    // convert to 16 bit pcm sound array
    // assumes the sample buffer is normalized.
    var idx = 0;
    values.foreach(sample => {
      // scale to maximum amplitude
      val value = (sample * 32767).toShort
      // in 16 bit wav PCM, first byte is the low order byte
      data(idx) = (value & 0x00ff).toByte
      idx += 1
      data(idx) = ((value & 0xff00) >>> 8).toByte
      idx += 1
    })
//    data
  }
  
//  val minimoogFilter = new MinimoogFilter
//  var loudnessCountur = new LoudnessContour(44100)
//  
//  val oscillator = new Oscillator(sampleRate)
  var mNote = 1

  def getGenerator(note: Int) = {
    Thread.currentThread setPriority (Thread.MIN_PRIORITY)

    var prevTime = 0
    val samples = new Array[Double](mNumSamples)
    val data = new Array[Byte](mNumSamples * 2)
    
//    oscillator.setGlide(3000)
          
    while (!m_stop && !Thread.currentThread.isInterrupted()) {
      val freqHz = C_FREQ * Math.pow(SEMITONE, mNote)
//      oscillator.processSamples(samples, freqHz)
////      lowPassFilter.processSamples(samples, 150, 440, FilterTypeLowPass, FilterSlope24)
//      minimoogFilter.processSamples(samples, 1000, 0.7)
//      loudnessCountur.processSamples(samples)
      valuesToSamples(samples, data)

      mAudioTrack.write(data, 0, data.length)
    }
  }
    
  private var mMidiListener = new MidiListener {

    def onNoteOff(channel: Int, note: Int, velocity: Int) = {
//        m_stop = true;          
//        m_audioTrack.stop();
//        if (m_noiseThread != null)
//        m_noiseThread.interrupt()
//      
    }

    def onNoteOn(channel: Int, note: Int, velocity: Int) = {
      onNoteOff(channel, note, velocity)
      
      m_stop = false;

      mAudioTrack.play();
      mNote = note
      
//      loudnessCountur.reset(500,500,5)
      
      if (mSoundThread == null) {

        //      Log d (TAG, ""+note)
        mSoundThread = new Thread(new Runnable {
          def run() {
            getGenerator(note)
          }
        })
        mSoundThread.start();
      }
    }

    def onNoteAftertouch(channel: Int, note: Int, aftertouch: Int) = ()

    def onController(channel: Int, control: Int, value: Int) = ()

    def onProgramChange(channel: Int, program: Int) = ()

    def onChannelAftertouch(channel: Int, aftertouch: Int) = ()

    def onPitchBend(channel: Int, value: Int) = ()
    def onTimingClock() = ()

    def onActiveSensing() = ()

    def onSequenceNumber(sequenceNumber: Int) = ()

    def onText(text: Array[Byte]) = ()

    def onCopyrightNotice(text: Array[Byte]) = ()

    def onSequenceName(text: Array[Byte]) = ()

    def onInstrumentName(text: Array[Byte]) = ()

    def onLyrics(text: Array[Byte]) = ()

    def onMarker(text: Array[Byte]) = ()

    def onCuePoint(text: Array[Byte]) = ()

    def onChannelPrefix(channel: Int) = ()

    def onPort(data: Array[Byte]) = ()

    def onEndOfTrack() = ()

    def onSetTempo(microsecondsPerQuarterNote: Int) = ()

    def onSmpteOffset(data: Array[Byte]) = ()

    def onTimeSignature(numerator: Int, denominator: Int,
      metronomePulse: Int, thirtySecondNotesPerQuarterNote: Int) = {
    }

    def onKeySignature(key: Int, isMinor: Boolean) = {
    }

    def onSequencerSpecificEvent(data: Array[Byte]) = {
    }

    def onSysEx(data: Array[Byte]) = {
    }
  }

  override def onCreate(savedInstanceState: Bundle) = {
    super.onCreate(savedInstanceState)
    setContentView(R.layout.activity_main)
  
    mPiano = findViewById(R.id.piano).asInstanceOf[PianoView]
    mPiano.bindTo(mMidiListener)
  }
}