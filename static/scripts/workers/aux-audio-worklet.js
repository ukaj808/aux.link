class AuxAudioWorklet extends AudioWorkletProcessor {

  #numChannels;
  #ringBuffer;
  #ringBufferSize
  #offset;
  #state;

  constructor(options) {
    super();
    // Create views on states shared buffer
    this.#ringBufferSize = options.processorOptions.ringBufferSize;
    this.#ringBuffer     = new Int16Array(options.processorOptions.ringBuffer);
    this.#state          = new Int8Array(options.processorOptions.state);
    this.#numChannels    = options.processorOptions.numChannels;
    this.#offset         = 0;
  }

  process(_inputs, outputs) {
    if (!this.#isAudioAvailable()) {
      return true;
    } 
    const output    = outputs[0]; // 1st output source
    const numFrames = output[0].length;
    for (let frame = 0; frame < numFrames; frame = frame + 2) {
      for (let channel = 0; channel < this.#numChannels; channel++) {
        const sampleIndex = (this.#offset + frame + channel) % (this.#ringBufferSize / Int16Array.BYTES_PER_ELEMENT);
        const sampleI16 = this.#ringBuffer[sampleIndex];
        const sampleF32 = sampleI16 / 32767;
        output[channel][frame] = sampleF32;
      }
    }
    this.#offset = this.#offset + ((numFrames * this.#numChannels) % (this.#ringBufferSize / Int16Array.BYTES_PER_ELEMENT));
    return true;

  }

  #isAudioAvailable(){
    return Atomics.load(this.#state, 0) == 1;
  }
}

registerProcessor("aux-audio-worklet", AuxAudioWorklet);
