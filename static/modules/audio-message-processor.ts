import { CurrentlyPlayingElement } from "./currently-playing/currently-playing-element";

export class AudioMessageProcessor {

  private currentlyPlayingElement: CurrentlyPlayingElement;

  constructor(currentlyPlayingElement: CurrentlyPlayingElement) {
    this.currentlyPlayingElement = currentlyPlayingElement;
  }
}
