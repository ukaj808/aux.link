import { OrderElement } from "./order-element";
import { RestClient } from "./rest-client";

export class RoomMessageListener {

  private roomId: string;
  private ws: WebSocket | null;
  private orderEl: OrderElement;
  private restClient: RestClient;

  constructor(roomId: string, orderEl: OrderElement, restClient: RestClient) {
    this.roomId = roomId;
    this.orderEl = orderEl;
    this.ws = null;
    this.restClient = restClient;
  }

  public start() {
    this.ws = new WebSocket(`ws://localhost:8080/rooms/${this.roomId}/ws`);
    this.ws.onmessage = this.process;
  }

  private process = (event: MessageEvent<string>) => {
    const data = JSON.parse(event.data) as RoomMessage;
    switch (data.type) {
        case "ServerWelcomeMessage":
            const welcomeMessage = data as ServerWelcomeMessage;
            this.restClient.setUserId(welcomeMessage.userId);
            this.orderEl.addNewUserToOrderCarousel(data.userId, welcomeMessage.userName, welcomeMessage.isCreator);
            break;
        case "UserEnterEvent":
            const userEnterEvent = data as UserEnterEvent;
            this.orderEl.addNewUserToOrderCarousel(data.userId, userEnterEvent.userName);
            break;
        case "UserLeftEvent":
            const userLeftEvent = data as UserLeftEvent;
            this.orderEl.removeUserFromOrderCarousel(data.userId);
            break;
        default:
            console.error("Unrecognized Event!");
            break;
    }
  }
}
