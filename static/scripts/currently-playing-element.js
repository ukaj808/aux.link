export class CurrentlyPlayingElement {

  #el;
  #auxAudioPlayer;
  #listening;

  constructor(auxAudioPlayer) {
    this.#el = document.getElementById("currently-playing");
    this.#listening = false;
    this.#auxAudioPlayer = auxAudioPlayer;
    this.#el.addEventListener("click", () => this.#onSectionClick());
  }

  #onSectionClick() {
    if (!this.#listening) {
        this.#auxAudioPlayer.startListening({userId: "0"});
        this.#listening = true;
        this.#toggleDisconnectOverlay();
    } else {
        this.#auxAudioPlayer.stopListening();
        this.#listening = false;
    }
  }

  #toggleDisconnectOverlay() {
    this.#el.classList.toggle("overlay");
  }

}
