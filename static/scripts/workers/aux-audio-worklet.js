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

class AuxAudioWorklet extends AudioWorkletProcessor {

  #channels;
  #states;
  #ringBuffer
  #ringBufferLength;
  #kernelLength;

  constructor(options) {
    console.log(options);
    super();
    // Create views on states shared buffer
    this.#states = new Int32Array(options.workletOptions.sharedBuffers.states); 
    this.#ringBuffer = [new Float32Array(options.workletOptions.sharedBuffers.ringBuffer)];

    this.#ringBufferLength = this.#states[STATE_INDICES.RING_BUFFER_LENGTH];
    this.#kernelLength = this.#states[STATE_INDICES.KERNEL_LENGTH];

    this.port.postMessage({type: "AUDIO_WORKLET_READY"});

  }

  process(_inputs, outputs) {

    return true;

  }
}

registerProcessor("aux-audio-worklet", AuxAudioWorklet);
