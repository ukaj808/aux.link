import { AuxAudioPlayer } from "../../../aux-audio-player";

export function fromCountdownToDisconnected(
    auxAudioPlayer: AuxAudioPlayer, 
    overlay: HTMLDivElement, 
    countdownTimer: HTMLSpanElement,
    disconnectBtn: HTMLButtonElement, 
    listeningFlag: boolean) {

    auxAudioPlayer.stopListening();
    overlay.classList.remove("invisible");
    countdownTimer.classList.add("hidden");

    disconnectBtn.classList.add("hidden");
    listeningFlag = false;

}