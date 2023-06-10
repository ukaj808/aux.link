import { DropElement } from "./drop-element";
import { OrderElement } from "./order-element";
import { RestClient } from "./rest-client";
import { SongQueue } from "./song-queue";

export class RoomMessageListener {

  private roomId: string;
  private ws: WebSocket | null;
  private orderEl: OrderElement;
  private dropEl: DropElement;
  private restClient: RestClient;

  constructor(roomId: string, orderEl: OrderElement, dropEl: DropElement, restClient: RestClient) {
    this.roomId = roomId;
    this.orderEl = orderEl;
    this.dropEl = dropEl;
    this.ws = null;
    this.restClient = restClient;
  }

  public start() {
    this.ws = new WebSocket(`ws://localhost:8080/rooms/${this.roomId}/ws`);
    this.ws.onmessage = this.process;
  }

  private process = (event: MessageEvent<string>) => {
    const data = JSON.parse(event.data) as RoomMessage;
    console.log(event);
    switch (data.type) {
        case "ServerWelcomeMessage":
            const welcomeMessage = data as ServerWelcomeMessage;
            this.restClient.setUserId(welcomeMessage.userId);
            this.orderEl.addNewUserToOrderCarousel(welcomeMessage.userId, welcomeMessage.userName, welcomeMessage.isCreator);
            break;
        case "ServerUploadSongMessage":
            this.dropEl.uploadAndDequeueSong();
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
        default:
            console.error("Unrecognized Event!");
            break;
    }
  }
}
