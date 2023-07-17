import { AuxAudioPlayer, AuxAudioPlayerEvent, StreamStartingEvent } from "../../aux-audio-player";
import { RoomView } from "../../interface";
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
    transitionTo: (state: CurrentlyPlayingState, data?: any) => void
    ){

    const onStreamConnected = (roomView: RoomView) => (data: AuxAudioPlayerEvent) => {
      listeningFlag = true;
      auxAudioPlayer.unsubscribe('STREAM_STARTING', onStreamConnected(roomView));
      loadingBars.classList.add("hidden");
      transitionTo(roomView.currentlyPlayingView.musicState, roomView);
    }

    overlay.classList.add("invisible");
    loadingBars.classList.remove("hidden");
    disconnectBtn.classList.remove("hidden");

    restClient.getRoom().then((room) => {
      auxAudioPlayer.subscribe('STREAM_CONNECTED', onStreamConnected(room));
      auxAudioPlayer.startListening();
    });
}