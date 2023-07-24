import { AuxAudioPlayer } from "../../../aux-audio-player";
import { RoomMessageListener } from "../../../room-message-listener";

export function fromPollingToDisconnected(
        roomMessageListener: RoomMessageListener,
        auxAudioPlayer: AuxAudioPlayer, 
        overlay: HTMLDivElement, 
        overlayClickHandler: () => void,
        songStartingEventHandler: (roomEvent: any) => void,
        songUploadedEventHandler: (roomEvent: any) => void,
        loadingBars: HTMLDivElement,
        disconnectBtn: HTMLButtonElement, 
        listeningFlag: boolean
    ) {
    roomMessageListener.unsubscribe('SongStartingEvent', songStartingEventHandler);
    roomMessageListener.unsubscribe('SongUploadedEvent', songUploadedEventHandler);
    overlay.addEventListener("click", overlayClickHandler);
    auxAudioPlayer.stopListening();
    overlay.classList.remove("invisible");
    loadingBars.classList.add("hidden");
    disconnectBtn.classList.add("hidden");
    listeningFlag = false;
}