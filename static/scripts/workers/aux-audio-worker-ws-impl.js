let roomId;
let userId;
let ws;
let ringBuffer;
let ringBufferSize
let chunkSize;
let state;
let offset = 0;

// Audios coming in every 1s; 1 second sized chunk
const onWsMessage = (event) => {
  const chunk = new Float32Array(event.data);
  ringBuffer.set(chunk, offset);
  offset = (offset + chunkSize) % ringBufferSize;
  Atomics.store(state, 0, 1);
  console.log('music available on ws');
};

self.onmessage = ({data}) => {
  if (data.type === "init") {
    // Create views on shared buffers
    ringBuffer = new Float32Array(data.ringBuffer);
    ringBufferSize = data.ringBufferSize;
    chunkSize = data.chunkSize;
    state = new Int8Array(data.state);
    // Init websocket connection
    roomId = data.roomId;
    userId = data.userId;

    ws = new WebSocket(`ws://localhost:8080/${roomId}/${userId}/music/listen`);
    ws.binaryType = 'arraybuffer';
    ws.addEventListener("message", onWsMessage); 
    postMessage({ type: 'WS_WORKER_READY' });
  } 
};
