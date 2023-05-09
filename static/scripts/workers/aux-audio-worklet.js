class AuxAudioWorklet extends AudioWorkletProcessor {

  #numChannels;
  #ringBuffer;
  #ringBufferSize
  #currentFrame;
  #state;

  constructor(options) {
    super();
    // Create views on states shared buffer
    this.#ringBufferSize = options.processorOptions.ringBufferSize;
    this.#ringBuffer     = new Float32Array(options.processorOptions.ringBuffer);
    this.#state          = new Int8Array(options.processorOptions.state);
    this.#currentFrame   = 0;
  }

  process(_inputs, outputs) {
    if (!this.#isAudioAvailable()) return true;
    console.log('audio avail to consume in worklet')

    const output    = outputs[0];
    const numFrames = output[0].length;

    for (let channel = 0; channel < this.#numChannels; channel++) {
      const outputChannel = output[channel];

      for (let i = 0; i < numFrames; i++) {
        const sampleIndex  = this.#currentFrame * this.#numChannels + channel;
        const sample       = this.#ringBuffer[sampleIndex];
        outputChannel[i]   = sample;
        this.#currentFrame = (this.#currentFrame + 1) % this.#ringBufferSize;
      }
    }

    return true;

  }

  #isAudioAvailable(){
    return Atomics.load(this.#state, 0) == 1;
  }
}

registerProcessor("aux-audio-worklet", AuxAudioWorklet);
