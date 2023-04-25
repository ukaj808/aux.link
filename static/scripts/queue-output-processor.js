import FreeQueue from "./free-queue";

class QueueOutputProcessor extends AudioWorkletProcessor {

  constructor(options) {
    super();
    this.audioQueue = options.audioQueue;
    this.RENDER_QUANTUM = 128;
    Object.setPrototypeOf(this.audioQueue, FreeQueue.prototype);
  }
  process(_inputs, outputs) {
    this.audioQueue.pull(outputs, this.RENDER_QUANTUM);
  }
}

registerProcessor("queue-output-processor", QueueOutputProcessor);
