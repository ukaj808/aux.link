import { AuxAudioPlayer, AuxAudioPlayerEvent, StreamStartingEvent } from "../../aux-audio-player";
import { RoomView } from "../../interface";
import { RestClient } from "../../rest-client";
import { CurrentlyPlayingElement, CurrentlyPlayingState } from "../currently-playing-element";

export function fromDisconnectedToConnecting(
    xState: CurrentlyPlayingState,
    restClient: RestClient,
    auxAudioPlayer: AuxAudioPlayer, 
    overlay: HTMLDivElement,
    overlayClickHandler: () => void,
    loadingBars: HTMLDivElement,
    disconnectBtn: HTMLButtonElement,
    listeningFlag: boolean,
    transitionTo: (state: CurrentlyPlayingState, data?: any) => void
    ){

    let onStreamConnected: ((data: AuxAudioPlayerEvent) => void) | undefined;

    const handleStreamConnected = (roomView: RoomView) => (data: AuxAudioPlayerEvent) => {
      if (onStreamConnected) {
        auxAudioPlayer.unsubscribe('STREAM_CONNECTED', onStreamConnected);
      }

      listeningFlag = true;
      loadingBars.classList.add("hidden");
      transitionTo(roomView.currentlyPlayingView.musicState, roomView);
    }

    overlay.removeEventListener("click", overlayClickHandler);

    overlay.classList.add("invisible");
    loadingBars.classList.remove("hidden");
    disconnectBtn.classList.remove("hidden");

    restClient.getRoom().then((room) => {
      onStreamConnected = handleStreamConnected(room);
      auxAudioPlayer.subscribe('STREAM_CONNECTED', onStreamConnected);
      auxAudioPlayer.startListening();
    });
}