import { OrderElement } from "./order-element";

export class RoomMessageListener {

  private roomId: string;
  private ws: WebSocket | null;
  private orderEl: OrderElement;

  constructor(roomId: string, orderEl: OrderElement) {
    this.roomId = roomId;
    this.orderEl = orderEl;
    this.ws = null;
  }

  public start() {
    this.ws = new WebSocket(`ws://localhost:8080/${this.roomId}/ws`);
    this.ws.onmessage = this.process;
  }

  private process = (event: MessageEvent<string>) => {
    const data = JSON.parse(event.data) as RoomMessage;
    switch (data.type) {
        case "ServerWelcomeMessage":
            const welcomeMessage = data as ServerWelcomeMessage;
            this.orderEl.addNewUserToOrderCarousel(data.userId, welcomeMessage.userName);
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
