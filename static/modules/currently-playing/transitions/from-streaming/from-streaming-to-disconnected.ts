import { AuxAudioPlayer } from "../../../aux-audio-player";
import { AudioVisualizer } from "../../audio-visualizer";

export function fromStreamingToDisconnected(
    auxAudioPlayer: AuxAudioPlayer, 
    audioVisualizer: AudioVisualizer,
    overlay: HTMLDivElement, 
    overlayClickHandler: () => void,
    description: HTMLSpanElement,
    disconnectBtn: HTMLButtonElement, 
    listeningFlag: boolean
    ) {
    overlay.addEventListener("click", overlayClickHandler);
    auxAudioPlayer.stopListening();
    audioVisualizer.stop();
    overlay.classList.remove("invisible");
    description.classList.add("hidden");
    disconnectBtn.classList.add("hidden");
    listeningFlag = false;
}