import { AuxAudioPlayer } from "../../../aux-audio-player";

export function fromPollingToDisconnected(
        auxAudioPlayer: AuxAudioPlayer, 
        overlay: HTMLDivElement, 
        overlayClickHandler: () => void,
        loadingBars: HTMLDivElement,
        disconnectBtn: HTMLButtonElement, 
        listeningFlag: boolean
    ) {
    overlay.addEventListener("click", overlayClickHandler);
    auxAudioPlayer.stopListening();
    overlay.classList.remove("invisible");
    loadingBars.classList.add("hidden");
    disconnectBtn.classList.add("hidden");
    listeningFlag = false;
}