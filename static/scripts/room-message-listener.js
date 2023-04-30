export class RoomMessageListener {

  #roomId;
  #ws;
  #userMessageProcessor;
  #audioMessageProcessor;

  constructor({roomId, userMessageProcessor, audioMessageProcessor}) {
    this.#roomId = roomId;
    this.#userMessageProcessor = userMessageProcessor;
    this.#audioMessageProcessor = audioMessageProcessor;
  }

  start() {
    this.#ws = new WebSocket(`ws://localhost:8080/${this.#roomId}/ws`);
    this.#ws.addEventListener("message", this.#distribute);
  }

  #distribute = ({data}) => {
    console.log(data);
    let parsedData = JSON.parse(data, this.#reviver);

    switch (parsedData?.topic) {
        case "user":
            this.#userMessageProcessor.procces(parsedData);
            createAndPublishServerWelcomeMessage(parsedData);
            break;
        case "audio":
            this.#audioMessageProcessor.process(parsedData);
            break;
        default:
            console.error("Unrecognized Event!");
            break;
    }
}

  #reviver = (key, value) => {
    if(typeof value === 'object' && value !== null) {
        if (value.dataType === 'Map') {
            return new Map(value.value);
        }
    }
    return value;
  }

}
