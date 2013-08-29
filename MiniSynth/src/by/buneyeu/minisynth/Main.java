package by.buneyeu.minisynth;

import by.buneyeu.minisynth.loudness.LoudnessContour;


public class Main {
	public static void main(String[] args) {
		int[] notes = new int[] { 60, 62, 64, 65, 67, 69, 71, 72, 72, 71, 69,
				67, 65, 64, 62, 60 };
		Synth synth = new Synth(44100);
		for (int note : notes) {
			System.out.println("synth.noteOn("+note+", 100);");
			synth.noteOn(note);
			try {
				Thread.sleep(200);
			} catch (InterruptedException e) {
				break;
			} finally {
				synth.noteOff(note);
			}
		}
		
//		LoudnessContour.getLoudnessPlot(100, 100, 0.7, 0, 300);
//		Test.plotFreq(44100, 200, 440, 600, 200);
//		Test.plotOscillator(44100, 440, 10);
//		Test.plotLoudness(100, 100, 0.7);
	}
}
