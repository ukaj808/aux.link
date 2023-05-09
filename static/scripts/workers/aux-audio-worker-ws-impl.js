let roomId;
let userId;
let ws;
let ringBuffer;
let ringBufferSize
let chunkSize;
let offset = 0;

const onWsMessage = (event) => {
  const chunk = new Float32Array(event.data);
  ringBuffer.set(chunk, offset);
  offset = offset + chunkSize;
  if (chunkSize == ringBufferSize) {
    index = 0;
  }
};

self.onmessage = ({data}) => {
  if (data.type === "init") {
    // Create views on shared buffers
    ringBuffer = new Float32Array(data.ringBuffer);
    ringBufferSize = data.ringBufferSize;
    chunkSize = data.chunkSize;
    // Init websocket connection
    roomId = data.roomId;
    userId = data.userId;

    ws = new WebSocket(`ws://localhost:8080/${roomId}/${userId}/music/listen`);
    ws.binaryType = 'arraybuffer';
    ws.addEventListener("message", onWsMessage); 
    postMessage({ type: 'WS_WORKER_READY' });
  } 
};
