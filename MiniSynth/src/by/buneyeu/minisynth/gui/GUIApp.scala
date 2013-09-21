package by.buneyeu.minisynth.gui

import scala.swing.SimpleSwingApplication
import scala.swing.MainFrame
import scala.swing.Button
import scala.swing.ComboBox
import scala.swing.Label
import scala.swing.BoxPanel
import scala.swing.Orientation
import scala.swing.Swing
import by.buneyeu.minisynth.oscillators.Oscillator._
import scala.swing.event.SelectionChanged
import by.buneyeu.minisynth.MinimoogSynth
import scala.collection.immutable.HashMap
import scala.swing.Slider
import scala.swing.event.ValueChanged
import scala.swing.CheckBox
import scala.swing.event.ButtonClicked

object GUIApp extends SimpleSwingApplication {

  import Waveform._

  val synth = new MinimoogSynth(44100)

  val waveforms = Waveform.values.toArray

  val TicksInSemitone = 1000
  val TicksInVolume = 1000
  
  def top = new MainFrame {
    title = "Hello, World!"
    val button = new Button {
      text = "Click me"
    }

    class OscBlock extends BoxPanel(Orientation.Horizontal) {
      val comboBox = new ComboBox[Waveform.Value](waveforms)
      contents += comboBox
      
      val multSlider = new Slider {
        min = -2
        max = 10
        value = 0
        paintTicks = true
        paintLabels = true
      }
      contents += multSlider

      val pitchSlider = new Slider {
        min = -7 * TicksInSemitone
        max = 7 * TicksInSemitone
        value = 0
        paintTicks = true
        paintLabels = true
      }
      contents += pitchSlider

      val volumeSlider = new Slider {
        min = 0
        max = 10 * TicksInVolume
        value = 0
        paintTicks = true
        paintLabels = true
      }
      contents += volumeSlider
    }

    val oscBlocks = Array(
      new OscBlock { // first oscillator hasn't pitch knob
        pitchSlider.visible = false
      },
      new OscBlock,
      new OscBlock)

    val syncCheckBox = new CheckBox {
      text = "Sync osc 2 to osc 1"
    }
    
    contents = new BoxPanel(Orientation.Vertical) {
      oscBlocks.foreach(contents += _)
      contents += syncCheckBox
      border = Swing.EmptyBorder(30, 30, 10, 30)
    }
    
    oscBlocks.foreach(osc => listenTo(osc.comboBox.selection))
    oscBlocks.foreach(osc => listenTo(osc.pitchSlider))
    oscBlocks.foreach(osc => listenTo(osc.volumeSlider))
    listenTo(syncCheckBox)
    
    var nClicks = 0
    reactions += {
      case SelectionChanged(component) => {
        val comboBox = component.asInstanceOf[ComboBox[Waveform.Value]]
        val oscIndex = oscBlocks.map(_.comboBox).indexOf(comboBox)
        synth.oscillators(oscIndex).waveform = comboBox.selection.item
      }
      case ValueChanged(component) => component match {
        case slider : Slider => {
          {
            val pitchSliderIndex = oscBlocks.map(_.pitchSlider).indexOf(slider)
            if (pitchSliderIndex != -1) {
              val semitones = slider.value.toDouble / TicksInSemitone
              System.out.println(semitones)
              synth.oscillators(pitchSliderIndex).pitch = semitones
            }
          }
          {
            val volumeSliderIndex = oscBlocks.map(_.volumeSlider).indexOf(slider)
            if (volumeSliderIndex != -1) {
              val volume = slider.value.toDouble / TicksInVolume
              synth.oscillators(volumeSliderIndex).volume = volume
            }
          }
        }
        case _ => throw new ClassCastException
      }
      case ButtonClicked(component) => component match {
        case `syncCheckBox` => {
          synth.oscillators(0).synchronized {
            synth.oscillators(0).synchronizedOscillator =
              if (syncCheckBox.selected)
                Option(synth.oscillators(1))
              else
                None
          }
        }
        case _ => new ClassCastException
      }
          
    }
  }
}