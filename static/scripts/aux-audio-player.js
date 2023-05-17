export class AuxAudioPlayer{
  #roomId;
  #userId;
  #audioContext;
  #wsWorker;
  #audioWorklet;

  constructor({roomId}) {
    this.#roomId = roomId;
  }

  async startListening({userId}) {
    this.#userId = userId;

    this.#audioContext = new AudioContext();
    this.#audioContext.suspend();
    console.info("Audio suspended...");

    await this.#audioContext.audioWorklet.addModule('public/aux-audio-worklet.js');

    const ringBufferSize = 1920000; // 10 Seconds of audio @ 192000 bytes per second
    const chunkSize = 24000; // 1/8th of a second of audio @ ...
    const ringBuffer = new SharedArrayBuffer(ringBufferSize);
    const state = new SharedArrayBuffer(1);

    this.#audioWorklet = new AudioWorkletNode(this.#audioContext, 'aux-audio-worklet', 
      { 
        outputChannelCount: [2],
        processorOptions: 
        {  
          ringBufferSize: ringBufferSize, // 3s of audio
          ringBuffer: ringBuffer,
          state: state,
          numChannels: 2
        } 
      });
    // Create Worker; pass sharedBuffers
    this.#wsWorker = new Worker('public/aux-audio-worker-ws-impl.js');
    this.#wsWorker.postMessage(
      { 
        type: "init", 
        roomId: this.#roomId, 
        userId: this.#userId, 
        ringBufferSize: ringBufferSize,
        ringBuffer: ringBuffer,
        state: state,
        chunkSize: chunkSize
      });
    console.info("Initializing ws worker...");

    this.#wsWorker.onmessage = async ({data}) => {
      if (data.type === 'WS_WORKER_READY') {
        console.info("Ws worker intialized...");

        this.#audioWorklet.connect(this.#audioContext.destination);
        console.info("Worklet connected to speakers...");

        this.#audioContext.resume();
        console.info("Audio resumed...");
      }

    }

  }

  stopListening() {
    this.#wsWorker.terminate();
  }

}
