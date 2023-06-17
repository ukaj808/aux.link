import { DropElement } from "./drop-element";
import { OrderElement } from "./order-element";
import { RestClient } from "./rest-client";
import { AuxAudioPlayer } from "./aux-audio-player";

export class RoomMessageListener {

  private roomId: string;
  private ws: WebSocket | null;
  private orderEl: OrderElement;
  private dropEl: DropElement;
  private restClient: RestClient;
  private auxAudioPlayer: AuxAudioPlayer;

  constructor(roomId: string, orderEl: OrderElement, dropEl: DropElement, restClient: RestClient, auxAudioPlayer: AuxAudioPlayer) {
    this.roomId = roomId;
    this.orderEl = orderEl;
    this.dropEl = dropEl;
    this.ws = null;
    this.restClient = restClient;
    this.auxAudioPlayer = auxAudioPlayer;
  }

  public start() {
    this.ws = new WebSocket(`ws://localhost:8080/${this.roomId}/ws`);
    this.ws.onmessage = this.process;
  }

  private process = (event: MessageEvent<string>) => {
    const data = JSON.parse(event.data) as RoomMessage;
    switch (data.type) {
        case "ServerWelcomeCommand":
            const welcomeMessage = data as ServerWelcomeCommand;
            this.restClient.setUserId(welcomeMessage.userId);
            this.auxAudioPlayer.setUserId(welcomeMessage.userId);
            this.orderEl.addNewUserToOrderCarousel(welcomeMessage.userId, welcomeMessage.userName, welcomeMessage.isCreator);
            break;
        case "ServerUploadSongCommand":
            this.dropEl.uploadAndDequeueSong();
            break;
        case "ServerPrepareAudioCommand":
            const prepareAudioCommand = data as ServerPrepareAudioCommand;
            this.auxAudioPlayer.prepAudioForNextSongStream({
              audioCtxOpts: {
                sampleRate: 48000,
                latencyHint: 'playback',
              },
              ringBufferSize: 123123,
            });
            break;
        case "UserEnterEvent":
            const userEnterEvent = data as UserEnterEvent;
            this.orderEl.addNewUserToOrderCarousel(userEnterEvent.userId, userEnterEvent.userName);
            break;
        case "UserLeftEvent":
            const userLeftEvent = data as UserLeftEvent;
            this.orderEl.removeUserFromOrderCarousel(userLeftEvent.userId);
            break;
        case "SongStartingEvent":
          const songStartingEvent = data as SongStartingEvent;
          console.log(`Song starting in ${songStartingEvent.s} seconds`)
          break;
        case "SongFinishedEvent":
          console.log(`Song finished!`);
          break;
        default:
            console.error("Unrecognized Event!");
            break;
    }
  }
}
