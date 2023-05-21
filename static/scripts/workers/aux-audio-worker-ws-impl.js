let roomId;
let userId;
let ws;
let ringBuffer;
let ringBufferSize;
let state;
let openState = false;
let offset = 0;

const onWsMessage = (event) => {
  const numSamples = event.data.byteLength / Int16Array.BYTES_PER_ELEMENT;

  const dataView = new DataView(event.data);

  for (let i = 0; i < numSamples; i++) {
    const ringIndex = (offset + i) % ringBufferSize;
    ringBuffer[ringIndex] = dataView.getInt16(i * Int16Array.BYTES_PER_ELEMENT, true);
  }

  offset = (offset + numSamples) % ringBufferSize;

  // Ring buffer is half full; allow worklet to start reading,
  if (!openState && offset >= ringBufferSize / 2) {
    openState = true;
    Atomics.store(state, 0, 1);
  }
};

const connectToAudioSocket = (roomId, userId) => {
    ws = new WebSocket(`ws://localhost:8080/${roomId}/${userId}/music/listen`);
    ws.binaryType = 'arraybuffer';
    ws.addEventListener("message", onWsMessage); 
}

self.onmessage = ({data}) => {
  if (data.type === "init") {
    // Create views on shared buffers
    ringBuffer     = new Int16Array(data.ringBuffer);
    ringBufferSize = ringBuffer.byteLength / Int16Array.BYTES_PER_ELEMENT;
    state          = new Int8Array(data.state);

    connectToAudioSocket(data.roomId, data.userId)

    postMessage({ type: 'WS_WORKER_READY' });
  } 

};

