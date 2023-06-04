import { AuxAudioPlayer } from "./aux-audio-player";
import { RestClient } from "./rest-client";

export class UserElement {

  private el: HTMLElement;
  private userId: string;
  private creator: boolean;
  private restClient: RestClient;

  constructor(restClient: RestClient, userId: string, creator: boolean) {
    const userEl = document.getElementById(userId);
    if (!userEl) throw new Error('No user element found');
    this.userId = userId;
    this.el = userEl;
    this.creator = creator;
    this.restClient = restClient;
    this.el.addEventListener("click", () => this.onUserClick());
  }

  private onUserClick() {
    if (!this.creator) return;
    this.restClient.startMusic().then(() => {
      // remove overlay
    }).catch((err) => {
      console.error("Error calling the start music api", err);
    })

  }

}
