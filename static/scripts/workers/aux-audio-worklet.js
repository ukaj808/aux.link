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
      console.log('Audio unavailable');
      return true;
    } 
    console.log('Audio available!');
    const output    = outputs[0]; // 1st output source
    const numFrames = output[0].length;
    for (let frame = 0; frame < numFrames; frame = frame + 2) {
      for (let channel = 0; channel < this.#numChannels; channel++) {
        const sampleIndex = this.#offset + frame + channel; // the offset in the ringbuffer + the current frame out of the num frames (iterates in 2s) + the channel in the frame
        const sampleI16 = this.#ringBuffer[sampleIndex];
        const sampleF32 = sampleI16 / 32767;
        if (sampleIndex % 100 == 0) console.log('sample', sampleF32);
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
