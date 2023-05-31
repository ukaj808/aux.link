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

    this.#audioContext = new AudioContext(
      {
        latencyHint: "playback", 
        sampleRate: 48000,
      }
    );
    this.#audioContext.suspend();
    console.info("Audio suspended...");

    await this.#audioContext.audioWorklet.addModule('public/audio_worklet_processor_bundle.js');

    const ringBufferSize = 1920000; // 5 Seconds of audio @ 384000 bytes per second
    const ringBuffer = new SharedArrayBuffer(ringBufferSize);
    const state = new SharedArrayBuffer(1);

    this.#audioWorklet = new AudioWorkletNode(this.#audioContext, 'audio-worklet-processor', 
      { 
        outputChannelCount: [2],
        processorOptions: 
        {  
          ringBuffer: ringBuffer,
          state: state
        } 
      });
    // Create Worker; pass sharedBuffers
    this.#wsWorker = new Worker('public/audio_socket_worker_bundle.js');
    this.#wsWorker.postMessage(
      { 
        type: "init", 
        roomId: this.#roomId, 
        userId: this.#userId, 
        ringBuffer: ringBuffer,
        state: state,
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
