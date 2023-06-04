import { AuxAudioPlayer } from "./aux-audio-player";

export class RestClient {

  private roomId: string;
  private userId: string;

  private basePath: string;

  constructor(roomId: string, userId: string) {
    this.roomId = roomId;
    this.userId = userId;
    this.basePath = `/${roomId}/${userId}`;
  }

  public startMusic(): Promise<Response> {
    return fetch(this.basePath + '/music/start', {
        method: 'PUT'
    });
  }

  public uploadSong(): Promise<Response> {
    return fetch(this.basePath + '/music/start', {
        method: 'PUT'
    });
  }

  public enqueueSong(): Promise<Response> {

    return fetch(this.basePath + '/music/start', {
            method: 'PUT'
        });
  }

  public removeSong(): Promise<Response> {

    return fetch(this.basePath + '/music/start', {
            method: 'PUT'
        });
  }

  
  public reprioritizeSong(): Promise<void> {

  }

  private toggleDisconnectOverlay() {
    this.el.classList.toggle("overlay");
  }

}