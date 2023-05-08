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
  bytesPerState: Int32Array.BYTES_PER_ELEMENT,
  bytesPerSample: Float32Array.BYTES_PER_ELEMENT,
  stateBufferLength: 16,
  ringBufferLength: 4096,
  kernelLength: 1024,
  channelCount: 1,
  waitTimeOut: 25000,
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

    // Allocate SABs
    const sharedBuffers = {
      states:
        new SharedArrayBuffer(CONFIG.stateBufferLength * CONFIG.bytesPerState),
      ringBuffer:
        new SharedArrayBuffer(CONFIG.ringBufferLength * CONFIG.channelCount * CONFIG.bytesPerSample)
    }

    console.info("Allocated shared array buffers...");

    // Init State
    const stateView = new Int32Array(sharedBuffers.states);
    Atomics.store(stateView, STATE_INDICES.RING_BUFFER_LENGTH, CONFIG.ringBufferLength);
    Atomics.store(stateView, STATE_INDICES.KERNEL_LENGTH, CONFIG.kernelLength);
    console.info("Initialized states buffer...");

    this.#audioContext = new AudioContext();
    this.#audioContext.suspend();
    console.info("Audio suspended...");

    await this.#audioContext.audioWorklet.addModule('public/aux-audio-worklet.js');

    console.info("Initializing audio worklet...");
    this.#audioWorklet = new AudioWorkletNode(this.#audioContext, 'aux-audio-worklet', { workletOptions: { sharedBuffers: sharedBuffers  } });

    this.#audioWorklet.onmessage = ({type}) => {
      if (type === 'AUDIO_WORKLET_READY') {
        console.info("Audio worklet initialiazed...");
        // Create Worker; pass sharedBuffers
        this.#wsWorker = new Worker('public/aux-audio-worker-ws-impl.js');
        this.#wsWorker.postMessage({ type: "init", roomId: this.#roomId, userId: this.#userId, sharedBuffers: sharedBuffers })
        console.info("Initializing ws worker...");

        this.#wsWorker.onmessage = async ({type}) => {
          if (type === 'WS_WORKER_READY') {
            console.info("Ws worker intialized...");

            this.#audioWorklet.connect(this.#audioContext.destination);
            console.info("Worklet connected to speakers...");

            this.#audioContext.resume();
            this.#listening = true;
            console.info("Audio resumed...");
          }

        }
      }
    }

  }

  stopListening() {
    this.#wsWorker.terminate();
    this.#listening = false;
  }

}
