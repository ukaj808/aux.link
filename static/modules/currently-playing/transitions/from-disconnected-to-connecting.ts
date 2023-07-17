import { AuxAudioPlayer, AuxAudioPlayerEvent, StreamStartingEvent } from "../../aux-audio-player";
import { RestClient } from "../../rest-client";
import { CurrentlyPlayingElement, CurrentlyPlayingState } from "../currently-playing-element";

export function fromDisconnectedToConnecting(
    xState: CurrentlyPlayingState,
    restClient: RestClient,
    auxAudioPlayer: AuxAudioPlayer, 
    overlay: HTMLDivElement,
    loadingBars: HTMLDivElement,
    disconnectBtn: HTMLButtonElement,
    listeningFlag: boolean,
    transitionTo: (state: CurrentlyPlayingState) => void
    ){

    const onStreamConnected = (data: AuxAudioPlayerEvent) => {
      const streamStartingEvent = data as StreamStartingEvent;
      auxAudioPlayer.unsubscribe('STREAM_STARTING', onStreamConnected);
      loadingBars.classList.add("hidden");
    }
    restClient.getRoom().then((room) => {
      
    });
    auxAudioPlayer.subscribe('STREAM_CONNECTED', onStreamConnected);
    overlay.classList.add("invisible");
    loadingBars.classList.remove("hidden");
    disconnectBtn.classList.remove("hidden");
    auxAudioPlayer.startListening();
    listeningFlag = true;
}