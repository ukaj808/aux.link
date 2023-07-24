import { AuxAudioPlayer } from "../../../aux-audio-player";
import { RoomMessage } from "../../../interface";
import { RoomMessageListener } from "../../../room-message-listener";
import { AudioVisualizer } from "../../audio-visualizer";

export function fromStreamingToDisconnected(
    roomMessageListener: RoomMessageListener,  
    auxAudioPlayer: AuxAudioPlayer, 
    audioVisualizer: AudioVisualizer,
    overlay: HTMLDivElement, 
    overlayClickHandler: () => void,  
    songStartingEventHandler: (roomEvent: RoomMessage) => void,
    songUploadedEventHandler: (roomEvent: RoomMessage) => void,
    description: HTMLSpanElement,
    disconnectBtn: HTMLButtonElement, 
    listeningFlag: boolean
    ) {
    roomMessageListener.unsubscribe('SongStartingEvent', songStartingEventHandler);
    roomMessageListener.unsubscribe('SongUploadedEvent', songUploadedEventHandler);
    overlay.addEventListener("click", overlayClickHandler);
    auxAudioPlayer.stopListening();
    audioVisualizer.stop();
    overlay.classList.remove("invisible");
    description.classList.add("hidden");
    disconnectBtn.classList.add("hidden");
    listeningFlag = false;
}