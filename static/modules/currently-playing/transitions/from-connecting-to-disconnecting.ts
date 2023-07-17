import { AuxAudioPlayer } from "../../aux-audio-player";

export function fromConnectingToDisconnected(
    auxAudioPlayer: AuxAudioPlayer, 
    overlay: HTMLDivElement,
    disconnectBtn: HTMLButtonElement,
    listeningFlag: boolean
    ){

    auxAudioPlayer.stopListening();
    overlay.classList.remove("invisible");
    disconnectBtn.classList.add("hidden");
    listeningFlag = false;
}