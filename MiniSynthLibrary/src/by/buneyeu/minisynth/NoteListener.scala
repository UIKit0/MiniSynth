package by.buneyeu.minisynth

trait NoteListener {

  type Ms = Double
  
  def noteOn(note: Integer, duration: Ms)

  def noteOff(note: Integer)

}