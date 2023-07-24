import { AuxAudioPlayer } from "../../../aux-audio-player";

export function fromConnectingToDisconnected(
    auxAudioPlayer: AuxAudioPlayer, 
    overlay: HTMLDivElement,
    overlayClickHandler: () => void,
    loadingBars: HTMLDivElement,
    disconnectBtn: HTMLButtonElement,
    listeningFlag: boolean
    ){

    overlay.addEventListener("click", overlayClickHandler);

    auxAudioPlayer.stopListening();
    overlay.classList.remove("invisible");
    disconnectBtn.classList.add("hidden");
    listeningFlag = false;
}