import { AuxAudioPlayer } from "../../../aux-audio-player";

export function fromCountdownToDisconnected(
    auxAudioPlayer: AuxAudioPlayer, 
    overlay: HTMLDivElement, 
    overlayClickHandler: () => void,
    countdownTimer: HTMLSpanElement,
    disconnectBtn: HTMLButtonElement, 
    listeningFlag: boolean) {

    overlay.addEventListener("click", overlayClickHandler);

    auxAudioPlayer.stopListening();
    overlay.classList.remove("invisible");
    countdownTimer.classList.add("hidden");

    disconnectBtn.classList.add("hidden");
    listeningFlag = false;

}