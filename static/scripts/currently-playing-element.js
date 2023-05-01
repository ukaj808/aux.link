export class CurrentlyPlayingElement {

  #el;
  #auxAudioPlayer;
  #listening;

  constructor({auxAudioPlayer}) {
    this.#el = document.getElementById("currently-playing");
    this.#el.addEventListener("click", () => this.#currentClickHandler());
    this.#listening = false;
    this.#auxAudioPlayer = auxAudioPlayer;
  }

  #currentClickHandler() {
    console.log(this.#listening);
    if (this.#listening == false) {
        this.#auxAudioPlayer.startListening({userId: "0"});
        this.#listening = true;
    } else {
        this.#auxAudioPlayer.stopListening();
        this.#listening = false;
    }
}

}
