import { RoomView } from "../../../interface";
import { AudioVisualizer } from "../../audio-visualizer";

export function fromConnectingToStreaming(description: HTMLSpanElement, audioVisualizer: AudioVisualizer, roomView: RoomView) {
    if (roomView.currentlyPlayingView.song === null) {
        throw new Error('Song is undefined');
    }
    description.innerText = roomView.currentlyPlayingView.song;
    description.classList.remove("hidden");
    audioVisualizer.start();
}