let roomId: string;
let userId: string;
let ws: WebSocket;
let ringBuffer: Float32Array;
let state: Int8Array;
let audioWorkletOffset: Int32Array;
let audioWorkletLap: Int32Array;
let openState: boolean = false;
let offset: Int32Array;
let lap: Int32Array;
let samplesRead: Int32Array; // distance
let samplesWritten: Int32Array; // distance
let lappedCount: number = 1;

const resetState = () => {
  ringBuffer.fill(0);
  offset[0] = 0;
  state[0] = 0;
  lap[0] = 0;
  openState = false;
  samplesWritten[0] = 0;
}

const onWsMessage = (event: MessageEvent<AudioChunk>) => {
  if (event.data.byteLength == 1) {
      const signal = new DataView(event.data).getInt8(0); 
      if (signal == 0) {  // 0 means the song is over 
        postMessage({ type: 'SONG_FINISHED' });
        resetState();
      } else if (signal == 1) { // 1 means the song started
        postMessage({ type: 'SONG_STARTED' });
      }
      return;
  }

  const data = new Float32Array(event.data);
  if (data.length <= ringBuffer.length - offset[0]) {
    // If there's enough space for the data, simply copy it to the ring buffer
    ringBuffer.set(data, offset[0]);
  } else {
    // If the data exceeds the space left in the ring buffer, wrap it around
    const remainingSpace = ringBuffer.length - offset[0];
    ringBuffer.set(data.subarray(0, remainingSpace), offset[0]);
    ringBuffer.set(data.subarray(remainingSpace), 0);
  }
  samplesWritten[0] = samplesWritten[0] + data.length;

  if ((samplesWritten[0] - samplesRead[0]) > (ringBuffer.length * lappedCount)) {
    lappedCount += 1;
    console.log(`Buffer overrun!`);
  }

  offset[0] = (offset[0] + data.length) % ringBuffer.length;
  
  // Ring buffer is half full; allow worklet to start reading,
  if (!openState) {
    openState = true;
    state[0] = 1;
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
    audioWorkletOffset = new Int32Array(data.audioWorkletOffset);
    audioWorkletLap = new Int32Array(data.audioWorkletLap);
    offset = new Int32Array(data.wsWorkerOffset);
    lap = new Int32Array(data.wsWorkerLap);
    samplesRead = new Int32Array(data.samplesRead);
    samplesWritten = new Int32Array(data.samplesWritten);

    connectToAudioSocket(data.roomId, data.userId)

    postMessage({ type: 'WS_WORKER_READY' });
  } 

};

