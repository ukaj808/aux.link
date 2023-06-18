import { AuxAudioPlayer } from "./aux-audio-player";

export class CurrentlyPlayingElement {

  private el: HTMLElement;
  private auxAudioPlayer: AuxAudioPlayer;
  private listening: boolean;

  constructor(auxAudioPlayer: AuxAudioPlayer) {
    const optEl = document.getElementById("currently-playing");
    if (!optEl) throw new Error('No currently playing element found');
    this.el = optEl;
    this.listening = false;
    this.auxAudioPlayer = auxAudioPlayer;
    this.el.addEventListener("click", () => this.onSectionClick());
  }

  private onSectionClick() {
    if (!this.listening) {
        this.auxAudioPlayer.startListening();
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
