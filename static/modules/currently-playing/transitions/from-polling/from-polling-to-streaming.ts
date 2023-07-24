import { StreamStartingEvent } from "../../../aux-audio-player";
import { SongUploadedEvent } from "../../../interface";
import { AudioVisualizer } from "../../audio-visualizer";

export function fromPollingToStreaming(
    loadingBars: HTMLDivElement, 
    description: HTMLSpanElement, 
    audioVisualizer: AudioVisualizer, 
    songUploadedEvent: SongUploadedEvent
    ) {

        loadingBars.classList.add("hidden");
        description.classList.remove("hidden");
        audioVisualizer.start();

        description.innerText = songUploadedEvent.title;

}