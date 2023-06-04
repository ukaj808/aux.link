let roomId: string;
let userId: string;
let ws: WebSocket;
let ringBuffer: Float32Array;
let state: Int8Array;
let openState: boolean = false;
let offset: number = 0;

const onWsMessage = (event: MessageEvent<AudioChunk>) => {

  const data = new Float32Array(event.data);

  ringBuffer.set(data, offset);

  offset = (offset + data.length) % ringBuffer.length;

  // Ring buffer is half full; allow worklet to start reading,
  if (!openState && offset >= ringBuffer.length / 2) {
    openState = true;
    Atomics.store(state, 0, 1);
  }
};

const connectToAudioSocket = (roomId: string, userId: string) => {
    ws = new WebSocket(`ws://localhost:8080/rooms/${roomId}/users/${userId}/music/listen`);
    ws.binaryType = 'arraybuffer';
    ws.addEventListener("message", onWsMessage); 
}

self.onmessage = (messageEvent: MessageEvent<WsWorkerOpts>) => {
  const data = messageEvent.data;
  if (data.type === "INIT") {
    // Create views on shared buffers
    ringBuffer     = new Float32Array(data.ringBuffer);
    state          = new Int8Array(data.state);

    connectToAudioSocket(data.roomId, data.userId)

    postMessage({ type: 'WS_WORKER_READY' });
  } 

};

