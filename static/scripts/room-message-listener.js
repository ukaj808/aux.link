export class RoomMessageListener {

  #roomId;
  #ws;
  #orderElement;

  constructor(roomId, orderElement) {
    this.#roomId = roomId;
    this.#orderElement = orderElement;
  }

  start() {
    this.#ws = new WebSocket(`ws://localhost:8080/${this.#roomId}/ws`);
    this.#ws.addEventListener("message", this.#process);
  }

  #process = (event) => {
    const data = JSON.parse(event.data);
    switch (data?.type) {
        case "ServerWelcomeMessage":
            this.#orderElement.addNewUserToOrderCarousel(data.userId, data.userName);
            break;
        case "UserEnterEvent":
            this.#orderElement.addNewUserToOrderCarousel(data.userId, data.userName);
            break;
        case "UserLeftEvent":
            this.#orderElement.removeUserFromOrderCarousel(data.userId, data.userName);
            break;
        default:
            console.error("Unrecognized Event!");
            break;
    }
  }
}
