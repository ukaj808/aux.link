import { RoomView } from "../../interface";
import { AudioVisualizer } from "../audio-visualizer";

export function fromConnectingToStreaming(description: HTMLSpanElement, audioVisualizer: AudioVisualizer, roomView: RoomView) {
    description.classList.remove("hidden");
    if (roomView.currentlyPlayingView.song === null) {
        throw new Error('Song is undefined');
    }
    audioVisualizer.start();
    description.innerText = roomView.currentlyPlayingView.song;
}