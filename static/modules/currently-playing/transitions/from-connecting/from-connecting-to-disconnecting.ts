import { AuxAudioPlayer } from "../../../aux-audio-player";
import { RoomMessage } from "../../../interface";
import { RoomMessageListener } from "../../../room-message-listener";

export function fromConnectingToDisconnected(
    roomMessageListener: RoomMessageListener,
    auxAudioPlayer: AuxAudioPlayer, 
    overlayClickHandler: () => void,
    songStartingEventHandler: (roomEvent: RoomMessage) => void,
    songUploadedEventHandler: (roomEvent: RoomMessage) => void,
    overlay: HTMLDivElement,
    loadingBars: HTMLDivElement,
    disconnectBtn: HTMLButtonElement,
    listeningFlag: boolean
    ){
    
    roomMessageListener.unsubscribe('SongStartingEvent', songStartingEventHandler);
    roomMessageListener.unsubscribe('SongUploadedEvent', songUploadedEventHandler);

    overlay.addEventListener("click", overlayClickHandler);

    auxAudioPlayer.stopListening();
    overlay.classList.remove("invisible");
    disconnectBtn.classList.add("hidden");
    listeningFlag = false;
}