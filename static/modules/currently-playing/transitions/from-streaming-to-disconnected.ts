import { AuxAudioPlayer } from "../../aux-audio-player";

export function fromStreamingToDisconnected(
    auxAudioPlayer: AuxAudioPlayer, 
    overlay: HTMLDivElement, 
    description: HTMLSpanElement,
    disconnectBtn: HTMLButtonElement, 
    listeningFlag: boolean
    ) {
    auxAudioPlayer.stopListening();
    overlay.classList.remove("invisible");
    description.classList.add("hidden");
    disconnectBtn.classList.add("hidden");
    listeningFlag = false;
}