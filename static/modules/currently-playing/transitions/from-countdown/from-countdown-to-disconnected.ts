import { AuxAudioPlayer } from "../../../aux-audio-player";
import { RoomMessageListener } from "../../../room-message-listener";

export function fromCountdownToDisconnected(
    roomMessageListener: RoomMessageListener,
    auxAudioPlayer: AuxAudioPlayer, 
    overlay: HTMLDivElement, 
    overlayClickHandler: () => void,
    songStartingEventHandler: (roomEvent: any) => void,
    songUploadedEventHandler: (roomEvent: any) => void,
    countdownTimer: HTMLSpanElement,
    disconnectBtn: HTMLButtonElement, 
    listeningFlag: boolean) {

    roomMessageListener.unsubscribe('SongStartingEvent', songStartingEventHandler);
    roomMessageListener.unsubscribe('SongUploadedEvent', songUploadedEventHandler);

    overlay.addEventListener("click", overlayClickHandler);

    auxAudioPlayer.stopListening();
    overlay.classList.remove("invisible");
    countdownTimer.classList.add("hidden");

    disconnectBtn.classList.add("hidden");
    listeningFlag = false;

}