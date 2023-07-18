import { RoomView } from "../../interface";

export function fromConnectingToStreaming(description: HTMLSpanElement, roomView: RoomView) {
    description.classList.remove("hidden");
    if (roomView.currentlyPlayingView.song === null) {
        throw new Error('Song is undefined');
    }
    description.innerText = roomView.currentlyPlayingView.song;
}