export class AuxAudioPlayer{

  #roomId;
  #userId;
  #audioContext;
  #listening;
  #worker;
  #processor;
  #processorOpts;
  #sharedBuffer;

  constructor({roomId}) {
    this.#roomId = roomId;
  }

  async startListening({userId}) {
    this.#userId = userId;
    this.#sharedBuffer = new SharedArrayBuffer(1024);
    this.#listening    = false;
    this.#worker = new Worker('public/aux-audio-worker-ws-impl.js');
    this.#audioContext = new AudioContext();
    await this.#audioContext.audioWorklet.addModule('public/aux-audio-worklet.js');

    this.#processorOpts = { channels: 2, sharedBuffer: this.#sharedBuffer };
    this.#processor = new AudioWorkletNode(this.#audioContext, 'aux-audio-worklet', this.#processorOpts);

    this.#worker.postMessage({ type: "init", roomId: this.#roomId, userId: this.#userId, sharedBuffer: this.#sharedBuffer })

    this.#processor.connect(this.#audioContext.destination);

    this.#listening = true;
  }

  stopListening() {
    this.#worker.terminate();
    this.#listening = false;
  }

}
