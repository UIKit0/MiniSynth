package by.buneyeu.minisynth

trait NoteListener {

  def noteOn(note: Int)

  def noteOff(note: Int)

}