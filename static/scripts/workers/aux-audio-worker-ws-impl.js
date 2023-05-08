const STATE_INDICES = {
  // Flag for Atomics.wait() and notify().
  'REQUEST_RENDER': 0,

  // Available frames in Input SAB.
  'IB_FRAMES_AVAILABLE': 1,

  // Read index of Input SAB.
  'IB_READ_INDEX': 2,

  // Write index of Input SAB.
  'IB_WRITE_INDEX': 3,

  // Available frames in Output SAB.
  'OB_FRAMES_AVAILABLE': 4,

  // Read index of Output SAB.
  'OB_READ_INDEX': 5,

  // Write index of Output SAB.
  'OB_WRITE_INDEX': 6,

  // Size of Input and Output SAB.
  'RING_BUFFER_LENGTH': 7,

  // Size of user-supplied processing callback.
  'KERNEL_LENGTH': 8,
};

let roomId;
let userId;
let ws;
let states;
let ringBuffer;
let ringBufferLength;
let kernelLength;

const onWsMessage = (event) => {
  console.log(event);
};

self.onmessage = ({data}) => {
  if (data.type === "init") {
    // Create views on shared buffers
    states = new Int32Array(data.sharedBuffers.states);
    ringBuffer = [new Float32Array(data.sharedBuffers.ringBuffer)];

    ringBufferLength = states[STATE_INDICES.RING_BUFFER_LENGTH];
    kernelLength = states[STATE_INDICES.KERNEL_LENGTH];

    // Init websocket connection
    roomId = data.roomId;
    userId = data.userId;

    ws = new WebSocket(`ws://localhost:8080/${roomId}/${userId}/audio/listen`);
    ws.binaryType = 'arraybuffer';
    ws.addEventListener("message", onWsMessage); 

    postMessage({ type: 'WS_WORKER_READY' });

  } else if (msg.data.type === 'destroy') {
    ws.removeEventListener(onWsMessage);
    ws.close();
    ws = null;
    sharedBuffer = null;
  }
};
