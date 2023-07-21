import { AuxAudioPlayer } from "../../../aux-audio-player";
import { AudioVisualizer } from "../../audio-visualizer";

export function fromStreamingToDisconnected(
    auxAudioPlayer: AuxAudioPlayer, 
    audioVisualizer: AudioVisualizer,
    overlay: HTMLDivElement, 
    description: HTMLSpanElement,
    disconnectBtn: HTMLButtonElement, 
    listeningFlag: boolean
    ) {
    auxAudioPlayer.stopListening();
    audioVisualizer.stop();
    overlay.classList.remove("invisible");
    description.classList.add("hidden");
    disconnectBtn.classList.add("hidden");
    listeningFlag = false;
}