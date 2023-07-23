import { RoomMessage, SongStartingEvent } from "../../../interface";
import { RoomMessageListener } from "../../../room-message-listener";
import { CurrentlyPlayingState } from "../../currently-playing-element";

export function fromPollingToCountdown(
    loadingBars: HTMLDivElement, 
    countdownTimer: HTMLSpanElement,
    roomMessageListener: RoomMessageListener,
    transitionTo: (tState: CurrentlyPlayingState) => void
    ) {

    const songStartingCallback = (event: RoomMessage) => {
        const songStartingEvent = event as SongStartingEvent;
        countdownTimer.innerText = songStartingEvent.s.toString();
        if (songStartingEvent.s === 0) {
            roomMessageListener.unsubscribe('SongStartingEvent', songStartingCallback);
            transitionTo('Polling');
        }
    };

    loadingBars.classList.add('hidden');
    countdownTimer.innerHTML = '5';
    countdownTimer.classList.remove('hidden');
    roomMessageListener.subscribe('SongStartingEvent', songStartingCallback);
}
