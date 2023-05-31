import { OrderElement } from "./order-element";

export class RoomMessageListener {

  private roomId: string;
  private ws: WebSocket;
  private orderEl: OrderElement;

  constructor(roomId: string, orderEl: OrderElement) {
    this.roomId = roomId;
    this.orderEl = orderEl;
  }

  public start() {
    console.log(this.roomId);
    this.ws = new WebSocket(`ws://localhost:8080/${this.roomId}/ws`);
    this.ws.addEventListener("message", this.process);
  }

  private process = (event: MessageEvent<any>) => {
    const data = JSON.parse(event.data);
    switch (data?.type) {
        case "ServerWelcomeMessage":
            this.orderEl.addNewUserToOrderCarousel(data.userId, data.userName);
            break;
        case "UserEnterEvent":
            this.orderEl.addNewUserToOrderCarousel(data.userId, data.userName);
            break;
        case "UserLeftEvent":
            this.orderEl.removeUserFromOrderCarousel(data.userId);
            break;
        default:
            console.error("Unrecognized Event!");
            break;
    }
  }
}
