let roomId: string;
let userId: string;
let ws: WebSocket;
let ringBuffer: Float32Array;
let state: Int8Array;
let openState: boolean = false;
let offset: number = 0;

const onWsMessage = (event: MessageEvent<AudioChunk>) => {
  /*
  if (event.data.byteLength == 1) {
    const second = new DataView(event.data).getInt8(0); 
      postMessage({ type: 'SONG_STARTING', timeLeftInSeconds: second });
      return;
  }
  */

  const data = new Float32Array(event.data);

  if (data.length <= ringBuffer.length - offset) {
    // If there's enough space for the data, simply copy it to the ring buffer
    ringBuffer.set(data, offset);
  } else {
    // If the data exceeds the space left in the ring buffer, wrap it around
    const remainingSpace = ringBuffer.length - offset;
    ringBuffer.set(data.subarray(0, remainingSpace), offset);
    ringBuffer.set(data.subarray(remainingSpace), 0);
  }

  offset = (offset + data.length) % ringBuffer.length;  
  console.log(offset);
  
  // Ring buffer is half full; allow worklet to start reading,
  if (!openState && offset >= ringBuffer.length / 2) {
    openState = true;
    Atomics.store(state, 0, 1);
  }
};

const connectToAudioSocket = (roomId: string, userId: string) => {
    ws = new WebSocket(`ws://localhost:8080/${roomId}/users/${userId}/music/listen`);
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

