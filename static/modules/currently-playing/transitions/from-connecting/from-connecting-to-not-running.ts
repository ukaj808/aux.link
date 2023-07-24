import { RoomMessage } from "../../../interface";
import { RoomMessageListener } from "../../../room-message-listener";

export function fromConnectingToNotRunning(
    roomMessageListener: RoomMessageListener, 
    songStartingEventHandler: (roomEvent: RoomMessage) => void, 
    description: HTMLSpanElement
    ) {
    roomMessageListener.subscribe('SongStartingEvent', songStartingEventHandler);
    description.classList.remove("hidden");
    description.innerText = "Waiting for the creator to start the music...";
}