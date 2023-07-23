import { StreamStartingEvent } from "../../../aux-audio-player";
import { AudioVisualizer } from "../../audio-visualizer";

export function fromPollingToStreaming(
    loadingBars: HTMLDivElement, 
    description: HTMLSpanElement, 
    audioVisualizer: AudioVisualizer, 
    data: StreamStartingEvent
    ) {

        loadingBars.classList.add("hidden");
        description.classList.remove("hidden");
        audioVisualizer.start();

        description.innerText = data.title;

}