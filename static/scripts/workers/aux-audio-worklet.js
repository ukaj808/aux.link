class AuxAudioWorklet extends AudioWorkletProcessor {

  #channels;
  #sharedBuffer;

  constructor(options) {
    super();
    this.#sharedBuffer = options.sharedBuffer; 
    this.#channels = options.channels;
  }

  process(_inputs, outputs) {

    return true;

  }
}

registerProcessor("aux-audio-worklet", AuxAudioWorklet);
