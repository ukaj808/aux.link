import { EventBus } from "./event-bus";

export class RoomMessageListener {

  private roomId: string;
  private ws: WebSocket | null = null;
  private eventBus: EventBus<RoomMessageType, RoomMessage> = new EventBus();

  constructor(roomId: string) {
    this.roomId = roomId;
  }

  public start() {
    this.ws = new WebSocket(`ws://localhost:8080/${this.roomId}/ws`);
    this.ws.onmessage = this.process;
  }

  public subscribe(msgType: RoomMessageType, callback: (event: RoomMessage) => void) {
    this.eventBus.subscribe(msgType, callback);
  }

  public unsubscribe(eventType: RoomMessageType, callback: (event: RoomMessage) => void) {
    this.eventBus.unsubscribe(eventType, callback);
  }

  private process = (event: MessageEvent<string>) => {
    const data = JSON.parse(event.data) as RoomMessage;
    this.eventBus.publish(data.type, data);
  }
}
