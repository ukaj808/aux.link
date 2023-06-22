let roomId: string;
let userId: string;
let ws: WebSocket;
let ringBuffer: Float32Array;
let state: Int8Array;
let readerOffset: Int32Array;
let offset: Int32Array;
let samplesRead: Int32Array; // distance
let samplesWritten: Int32Array; // distance
let lappedCount: number = 1;
let offsetBreak: Int32Array;

const resetState = () => {
  offset[0] = 0;
  lappedCount = 1;
}

const onWsMessage = (event: MessageEvent<AudioChunk>) => {
  if (event.data.byteLength == 1) {
      const signal = new DataView(event.data).getInt8(0); 
      if (signal == 0) {  // 0 means the song is over 
        postMessage({ type: 'WRITE_SONG_FINISHED', offset: offset[0] });
        offsetBreak[0] = offset[0];
        ringBuffer.fill(0, offset[0]); // clear the rest of the ring buffer after the last audio chunk
        resetState();
      } else if (signal == 1) { // 1 means the song started
        // Clear samples written on new song start
        // because the audio worklet still references the old song's samples written to check
        // for buffer underruns
        samplesWritten[0] = 0;
        postMessage({ type: 'WRITE_SONG_STARTED' });
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
  console.log(`Samples written: ${samplesWritten[0]}`, `Samples read: ${samplesRead[0]}`);
  if ((samplesWritten[0] - samplesRead[0]) > (ringBuffer.length * lappedCount)) {
    lappedCount += 1;
    console.log(`Buffer overrun!`);
  }

  offset[0] = (offset[0] + data.length) % ringBuffer.length;
  
  // Ring buffer is half full; allow worklet to start reading,
  if (state[0] == 0 && (samplesWritten[0] > (ringBuffer.length / 2))) {
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
    readerOffset = new Int32Array(data.readerOffset);
    offset = new Int32Array(data.writerOffset);
    samplesRead = new Int32Array(data.samplesRead);
    samplesWritten = new Int32Array(data.samplesWritten);
    offsetBreak = new Int32Array(data.break);
    offsetBreak[0] = -1;

    connectToAudioSocket(data.roomId, data.userId)

    postMessage({ type: 'WS_WORKER_READY' });
  } 

};

