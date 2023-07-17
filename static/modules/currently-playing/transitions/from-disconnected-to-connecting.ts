import { AuxAudioPlayer, AuxAudioPlayerEvent, StreamStartingEvent } from "../../aux-audio-player";

export function fromDisconnectedToConnecting(
    auxAudioPlayer: AuxAudioPlayer, 
    overlay: HTMLDivElement,
    loadingBars: HTMLDivElement,
    disconnectBtn: HTMLButtonElement,
    listeningFlag: boolean
    ){

    const onStreamConnected = (data: AuxAudioPlayerEvent) => {
      const streamStartingEvent = data as StreamStartingEvent;
      auxAudioPlayer.unsubscribe('STREAM_STARTING', onStreamConnected);
      loadingBars.classList.add("hidden");
      // route logic??
    }
    auxAudioPlayer.subscribe('STREAM_CONNECTED', onStreamConnected);
    overlay.classList.add("invisible");
    loadingBars.classList.remove("hidden");
    disconnectBtn.classList.remove("hidden");
    auxAudioPlayer.startListening();
    listeningFlag = true;
}