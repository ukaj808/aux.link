const STATE_INDICES = {
  'LOCK': 0,
  'HAS_AUDIO': 1,
}

const STATE_VALUES = {
  'LOCK': {'LOCKED': 1, 'UNLOCKED': 0},
  'HAS_AUDIO': { 'YES': 1, 'NO': 0 }
}

class AuxAudioWorklet extends AudioWorkletProcessor {

  #channels;
  #chunkStates;
  #ringBuffer;
  #index = 0;

  constructor(options) {
    super();
    // Create views on states shared buffer
    for (let i = 0; i < chunkCount; i++) {
      this.#chunkStates.push(new Int32Array(options.processorOptions.sharedBuffers.chunkStates[i]));
      this.#ringBuffer.push(new Float32Array(options.processorOptions.sharedBuffers.ringBuffer[i]));
    }

    this.port.postMessage({type: "AUDIO_WORKLET_READY"});
  }

  process(_inputs, outputs) {

    if (Atomics.load(
      this.#chunkStates[this.#index], 
      STATE_INDICES.HAS_AUDIO) == STATE_VALUES.HAS_AUDIO.YES
    ) {
      Atomics.load(this.#ringBuffer[this.#index],
    }

    return true;

  }
}

registerProcessor("aux-audio-worklet", AuxAudioWorklet);
