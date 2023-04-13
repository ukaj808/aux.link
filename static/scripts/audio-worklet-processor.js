class AudioProcessor extends AudioWorkletProcessor {

    constructor() {
        super();
        this.port.onmessage = (event) => {
          this.audioBuffer = event.data;
        };
    }

  process(inputs, outputs, parameters) {
    const output = outputs[0];
    if (!this.audioBuffer || this.audioBuffer.length === 0) {
      return true; // Keep the processor alive
    }

    for (let channel = 0; channel < output.length; ++channel) {
      const outputChannel = output[channel];
      if (this.audioBuffer[channel]) {
        outputChannel.set(this.audioBuffer[channel]);
      } else {
        outputChannel.fill(0);
      }
    }

    // Remove the played data from the buffer
    this.audioBuffer.shift();
    return true; // Keep the processor alive
  }
}

registerProcessor('audio-processor', AudioProcessor);

