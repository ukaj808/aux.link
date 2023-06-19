class AuxWorkletProcessor extends AudioWorkletProcessor {

  private ringBuffer: DataView;
  private ringBufferSize: number;
  private offset: Int32Array;
  private lap: Int32Array;
  private wsWorkerOffset: Int32Array;
  private wsWorkerLap: Int32Array;
  private state: Int8Array;

  constructor(options: AudioWorkletNodeOptions) {
    super();
    // Create views on states shared buffer
    this.ringBuffer     = new DataView(options.processorOptions.ringBuffer);
    this.ringBufferSize = options.processorOptions.ringBuffer.byteLength;
    this.state          = new Int8Array(options.processorOptions.state);
    this.offset         = new Int32Array(options.processorOptions.audioWorkletOffset);
    this.lap            = new Int32Array(options.processorOptions.audioWorkletLap);
    this.wsWorkerOffset = new Int32Array(options.processorOptions.wsWorkerOffset);
    this.wsWorkerLap    = new Int32Array(options.processorOptions.wsWorkerLap);
  }

  process(_inputs: Float32Array[][], outputs: Float32Array[][]) {
    if (!this.isAudioAvailable()) {
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
        const calcPcmSampleIndex = ((this.offset[0] + pcmSample) * Float32Array.BYTES_PER_ELEMENT) % this.ringBufferSize;
        outputChannel[sample] = this.ringBuffer.getFloat32(calcPcmSampleIndex, true);
      }
    }
    
    const newOffset = (this.offset[0] + totalSamplesProcessed) % (this.ringBufferSize / Float32Array.BYTES_PER_ELEMENT);
    this.offset[0] = newOffset;

    if (newOffset == 0) {
      this.lap[0] = this.lap[0] + 1;
      console.log ("Reader Lap: " + this.lap[0]);
    }  

    return true;

  }

  private isAudioAvailable(){
    return this.state[0] == 1;
  }
}

registerProcessor("audio-worklet-processor", AuxWorkletProcessor);
