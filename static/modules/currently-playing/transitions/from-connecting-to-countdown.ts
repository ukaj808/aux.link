import { RoomMessage, SongStartingEvent } from "../../interface";
import { RoomMessageListener } from "../../room-message-listener";

export function fromConnectingToCountdown(countdownTimer: HTMLSpanElement, roomMessageListener: RoomMessageListener) {
    countdownTimer.innerHTML = '5';
    countdownTimer.classList.remove("hidden");

    const songStartingCallback = (event: RoomMessage) => {
        const songStartingEvent = event as SongStartingEvent;
        if (songStartingEvent.s === 0) {
            roomMessageListener.unsubscribe('SongStartingEvent', songStartingCallback);
        }
        countdownTimer.innerText = songStartingEvent.s.toString();
    };

    roomMessageListener.subscribe('SongStartingEvent', songStartingCallback);
}