class AuxAudioWorklet extends AudioWorkletProcessor {

  #ringBuffer;
  #ringBufferSize
  #offset;
  #state;

  constructor(options) {
    super();
    // Create views on states shared buffer
    this.#ringBuffer     = new DataView(options.processorOptions.ringBuffer);
    this.#ringBufferSize = options.processorOptions.ringBuffer.byteLength;
    this.#state          = new Int8Array(options.processorOptions.state);
    this.#offset         = 0;
  }

  process(_inputs, outputs) {
    if (!this.#isAudioAvailable()) {
      return true;
    } 
    const output    = outputs[0]; // 1st output source
    const numChannels = output.length;

    let totalSamplesProcessed = 0;

    for (let channel = 0; channel < numChannels; channel++) {
      const outputChannel = output[channel];
      const numSamples = outputChannel.length;
      totalSamplesProcessed += numSamples;
      for (let sample = 0, pcmSample = channel; sample < numSamples; sample++, pcmSample += numChannels) {
        const calcPcmSampleIndex = ((this.#offset + pcmSample) * Float32Array.BYTES_PER_ELEMENT) % this.#ringBufferSize;
        outputChannel[sample] = this.#ringBuffer.getFloat32(calcPcmSampleIndex, true);
      }
    }

    this.#offset = (this.#offset + totalSamplesProcessed) % this.#ringBufferSize;

    return true;

  }

  #isAudioAvailable(){
    return Atomics.load(this.#state, 0) == 1;
  }
}

registerProcessor("aux-audio-worklet", AuxAudioWorklet);
