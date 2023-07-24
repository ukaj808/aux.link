import { AuxAudioPlayer } from "../../../aux-audio-player";

export function fromNotRunningToDisconnected(
  auxAudioPlayer: AuxAudioPlayer,
  overlay: HTMLDivElement,
  overlayClickHandler: () => void,
  description: HTMLSpanElement,
  disconnectBtn: HTMLButtonElement,
  listeningFlag: boolean
) {
  overlay.addEventListener("click", overlayClickHandler);
  auxAudioPlayer.stopListening();
  overlay.classList.remove("invisible");
  description.classList.add("hidden");
  disconnectBtn.classList.add("hidden");
  listeningFlag = false;
}