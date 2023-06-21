class AuxWorkletProcessor extends AudioWorkletProcessor {

  // The shared ring buffer. Can only be read from in the worklet
  // EXCEPT for the case where the song is over (break is enabled)
  private ringBuffer: DataView;
  private ringBufferSize: number;

  // The read offset (or head pointer of this consumer, if you want to think of it that way)
  // Incremented after each iteration of process()
  private offset: Int32Array;

  // The state of the ring buffer. 0 means the ring buffer is not ready to be read from.
  // 0 or 1
  private state: Int8Array;

  // Current number of samples read by this consumer (reset after each song) 
  // Useful for detecting buffer overruns/underruns. 
  private samplesRead: Int32Array;

  // Read-only view of the number of samples written by the producer (ws worker)
  // Useful for detecting buffer overruns/underruns. 
  private _samplesWritten: Int32Array;

  // The number of times the writer offset (head pointer) has lapped the reader offset (head pointer)
  private lappedCount: number;

  // When a song is finished, this is set to the offset of the last sample of the song (by the ws worker)  
  // Which enables a graceful exit from the song (song won't be cut off)
  private break: number | null;

  constructor(options: AudioWorkletNodeOptions) {
    super();
    this.ringBuffer     = new DataView(options.processorOptions.ringBuffer);
    this.ringBufferSize = options.processorOptions.ringBuffer.byteLength / Float32Array.BYTES_PER_ELEMENT;
    this.state          = new Int8Array(options.processorOptions.state);
    this.offset         = new Int32Array(options.processorOptions.audioWorkletOffset);
    this.samplesRead    = new Int32Array(options.processorOptions.samplesRead);
    this._samplesWritten = new Int32Array(options.processorOptions.samplesWritten);
    this.lappedCount    = 1;
    this.break          = null;
    this.port.onmessage = this.onPostMessage;
  }

  private onPostMessage = (message: MessageEvent<AudioWorkletCommand>) => { 
    switch (message.data.type) {
      case 'WRITE_SONG_STARTED': {
        break;
      }
      case 'WRITE_SONG_FINISHED': {
        this.break = message.data.offset;
        break;
      }
    }
  }

  private resetState() {
    this.offset[0] = 0;
    this.samplesRead[0] = 0;
    this.state[0] = 0;
    this.lappedCount = 1;
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
        const calcPcmSampleIndex = (this.offset[0] + pcmSample) % this.ringBufferSize;
        const dataViewIndex = calcPcmSampleIndex * Float32Array.BYTES_PER_ELEMENT;
        outputChannel[sample] = this.ringBuffer.getFloat32(dataViewIndex, true);

        // Graceful exit if the song is over
        if (this.break !== null) {
          // consume all samples until the break offset
          this.ringBuffer.setFloat32(dataViewIndex, 0, true);
          if (calcPcmSampleIndex === this.break) {
            this.resetState();
            this.break = null;
            postMessage({ type: 'READ_SONG_FINISHED' });
          }
        }

      }
    }
    
    this.offset[0] = (this.offset[0] + totalSamplesProcessed) % this.ringBufferSize;
    this.samplesRead[0] = this.samplesRead[0] + totalSamplesProcessed;

    if ((this.samplesRead[0] - this._samplesWritten[0]) > (this.ringBufferSize * this.lappedCount)) {
      this.lappedCount += 1;
      console.log("Buffer under run!");
    }

    return true;

  }

  private isAudioAvailable(){
    return this.state[0] == 1;
  }
}

registerProcessor("audio-worklet-processor", AuxWorkletProcessor);
