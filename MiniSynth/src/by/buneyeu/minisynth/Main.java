package by.buneyeu.minisynth;


public class Main {
	public static void main(String[] args) {
		int[] notes = new int[] { 60, 62, 64, 65, 67, 69, 71, 72, 72, 71, 69,
				67, 65, 64, 62, 60 };
		Synth synth = new Synth(44100);
		for (int note : notes) {
			System.out.println("synth.noteOn("+note+", 100);");
			synth.noteOn(note, 100);
			try {
				Thread.sleep(200);
			} catch (InterruptedException e) {
				break;
			} finally {
				synth.noteOff(note);
			}
		}
	}
}
