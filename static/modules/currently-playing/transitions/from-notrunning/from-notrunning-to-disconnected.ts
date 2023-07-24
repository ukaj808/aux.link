import { AuxAudioPlayer } from "../../../aux-audio-player";
import { RoomMessageListener } from "../../../room-message-listener";

export function fromNotRunningToDisconnected(
  roomMessageListener: RoomMessageListener,
  auxAudioPlayer: AuxAudioPlayer,
  overlay: HTMLDivElement,
  overlayClickHandler: () => void,
  songStartingEventHandler: (roomEvent: any) => void,
  songUploadedEventHandler: (roomEvent: any) => void,
  description: HTMLSpanElement,
  disconnectBtn: HTMLButtonElement,
  listeningFlag: boolean
) {
  roomMessageListener.unsubscribe('SongStartingEvent', songStartingEventHandler);
  roomMessageListener.unsubscribe('SongUploadedEvent', songUploadedEventHandler);
  overlay.addEventListener("click", overlayClickHandler);
  auxAudioPlayer.stopListening();
  overlay.classList.remove("invisible");
  description.classList.add("hidden");
  disconnectBtn.classList.add("hidden");
  listeningFlag = false;
}