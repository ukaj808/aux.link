let roomId;
let userId;
let ws;
let ringBuffer;
let ringBufferSizeInBytes;
let ringBufferSizeInArrayElements;
let chunkSizeInBytes;
let chunkSizeInArrayElements;
let state;
let offset = 0;

const onWsMessage = (event) => {

  // Complexity arising from raw data being little_endian format
  // Determine the number of int16 values in the ArrayBuffer
  const int16Count = event.data.byteLength / Int16Array.BYTES_PER_ELEMENT;

  // Create a DataView for the ArrayBuffer
  const dataView = new DataView(event.data);

  // Read int16 values using DataView with little-endian endianness
  for (let i = 0; i < int16Count; i++) {
    ringBuffer[offset + i] = dataView.getInt16(i * Int16Array.BYTES_PER_ELEMENT, true);
    offset = (offset + 1) % ringBufferSizeInArrayElements;
  }

  console.log('ws worker offset', offset);

  // Half the buffer is full
  if (offset >= ringBufferSizeInArrayElements / 2) {
    Atomics.store(state, 0, 1);
  }
};

self.onmessage = ({data}) => {
  if (data.type === "init") {
    // Create views on shared buffers
    ringBuffer = new Int16Array(data.ringBuffer);
    ringBufferSizeInBytes = data.ringBufferSize;
    ringBufferSizeInArrayElements = ringBufferSizeInBytes / Int16Array.BYTES_PER_ELEMENT;
    chunkSize = data.chunkSize;
    chunkSizeInArrayElements = data.chunkSize / Int16Array.BYTES_PER_ELEMENT;
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
