import { AuxAudioPlayer } from "./aux-audio-player";

export class CurrentlyPlayingElement {

  private el: HTMLElement;
  private auxAudioPlayer: AuxAudioPlayer;
  private listening: boolean;

  constructor(auxAudioPlayer: AuxAudioPlayer) {
    this.el = document.getElementById("currently-playing");
    this.listening = false;
    this.auxAudioPlayer = auxAudioPlayer;
    this.el.addEventListener("click", () => this.onSectionClick());
  }

  private onSectionClick() {
    if (!this.listening) {
        this.auxAudioPlayer.startListening("0");
        this.listening = true;
        this.toggleDisconnectOverlay();
    } else {
        this.auxAudioPlayer.stopListening();
        this.listening = false;
    }
  }

  private toggleDisconnectOverlay() {
    this.el.classList.toggle("overlay");
  }

}
