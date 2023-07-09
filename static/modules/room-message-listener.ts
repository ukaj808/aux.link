export class RoomMessageListener {

  private roomId: string;
  private ws: WebSocket | null = null;
  private subscriptions: Map<RoomMessageType, ((event: RoomMessage) => void)[]> = new Map();

  constructor(roomId: string) {
    this.roomId = roomId;
  }

  public start() {
    this.ws = new WebSocket(`ws://localhost:8080/${this.roomId}/ws`);
    this.ws.onmessage = this.process;
  }

  public subscribe(msgType: RoomMessageType, callback: (event: RoomMessage) => void) {
    if (!this.subscriptions.has(msgType)) {
      this.subscriptions.set(msgType, []);
    }
    this.subscriptions.get(msgType)!.push(callback);
  }

  public unsubscribe(eventType: RoomMessageType, callback: (event: RoomMessage) => void) {
    if (!this.subscriptions.has(eventType)) {
      return;
    }
    const callbacks = this.subscriptions.get(eventType)!;
    const index = callbacks.indexOf(callback);
    if (index >= 0) {
      callbacks.splice(index, 1);
    }
  }

  private process = (event: MessageEvent<string>) => {
    const data = JSON.parse(event.data) as RoomMessage;
    const callbacks = this.subscriptions.get(data.type);
    if (callbacks) {
      callbacks.forEach(callback => callback(data));
    }
  }
}
