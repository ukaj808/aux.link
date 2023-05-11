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

  // Complexity arising from raw data being little_endian format
  // Determine the number of float values in the ArrayBuffer
  const floatCount = event.data.byteLength / Float32Array.BYTES_PER_ELEMENT;

  // Create a DataView for the ArrayBuffer
  const dataView = new DataView(event.data);

  // Read float values using DataView with little-endian endianness
  const chunk = new Float32Array(floatCount);
  for (let i = 0; i < floatCount; i++) {
    chunk[i] = dataView.getFloat32(i * Float32Array.BYTES_PER_ELEMENT, true);
  }
  ringBuffer.set(chunk, offset);
  offset = (offset + chunkSize / Float32Array.BYTES_PER_ELEMENT) % (ringBufferSize / Float32Array.BYTES_PER_ELEMENT);
  Atomics.store(state, 0, 1);
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
