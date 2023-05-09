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

const CONFIG = {
  chunkCount: 5
, chunkSize: 44100
};

export class AuxAudioPlayer{
  #roomId;
  #userId;
  #audioContext;
  #listening;
  #wsWorker;
  #audioWorklet;

  constructor({roomId}) {
    this.#roomId = roomId;
  }

  async startListening({userId}) {
    this.#userId = userId;

    const sharedBuffers = {
      chunkStates: [],
      ringBuffer: []
    }

    console.info("Allocated shared array buffers...");
    for (let i = 0; i <=  CONFIG.chunkCount; i++) {
      sharedBuffers.chunkStates.push(new SharedArrayBuffer(2));
      sharedBuffers.ringBuffer.push(new SharedArrayBuffer(CONFIG.chunkSize))
    }

    this.#audioContext = new AudioContext();
    this.#audioContext.suspend();
    console.info("Audio suspended...");

    await this.#audioContext.audioWorklet.addModule('public/aux-audio-worklet.js');

    this.#audioWorklet = new AudioWorkletNode(this.#audioContext, 'aux-audio-worklet', 
      { 
        processorOptions: 
        {  
          chunkCount: CONFIG.chunkCount, 
          sharedBuffers: sharedBuffers 
        } 
      });
    // Create Worker; pass sharedBuffers
    this.#wsWorker = new Worker('public/aux-audio-worker-ws-impl.js');
    this.#wsWorker.postMessage(
      { 
        type: "init", 
        roomId: this.#roomId, 
        userId: this.#userId, 
        chunkCount: CONFIG.chunkCount, 
        sharedBuffers: sharedBuffers 
      });
    console.info("Initializing ws worker...");

    this.#wsWorker.onmessage = async ({data}) => {
      if (data.type === 'WS_WORKER_READY') {
        console.info("Ws worker intialized...");

        this.#audioWorklet.connect(this.#audioContext.destination);
        console.info("Worklet connected to speakers...");

        this.#audioContext.resume();
        this.#listening = true;
        console.info("Audio resumed...");
      }

    }

  }

  stopListening() {
    this.#wsWorker.terminate();
    this.#listening = false;
  }

}
