export class AuxAudioPlayer {
  private roomId: string;
  private userId?: string;
  private audioContext?: AudioContext;
  private wsWorker?: Worker;
  private audioWorklet?: AudioWorkletNode;
  private ringBufferSize?: number;
  private ringBuffer?: SharedArrayBuffer;
  private state?: SharedArrayBuffer;

  constructor(roomId: string) {
    this.roomId = roomId;
    this.ringBufferSize = 1920000;
  }

  public setUserId(userId: string) {
    this.userId = userId;
  }

  public async startListening() {
    if (this.userId === undefined) throw new Error("Audio context wasnt initialized");
    if (this.ringBufferSize === undefined) throw new Error("Ring buffer size wasnt initialized");

    this.audioContext = new AudioContext(
      {
        latencyHint: "playback", 
        sampleRate: 48000,
      }
    );
    this.audioContext.suspend();

    // we might want to refactor the audio context construction to be done in reaction/accordance to the song starting event
    // Since we might want to support different sample rates for different scenarios 
    // (e.g. the user has low bandwidth and should receive a lower quality stream)

    // This also might need to be refactored to be set in reaction/accordance to the song starting event
    this.ringBuffer = new SharedArrayBuffer(this.ringBufferSize);
    this.state = new SharedArrayBuffer(1);

    this.wsWorker = new Worker('public/audio_socket_worker_bundle.js');
    this.wsWorker.onmessage = this.onPostMessage;

    await this.audioContext.audioWorklet.addModule('public/audio_worklet_processor_bundle.js');

    this.audioWorklet = new AudioWorkletNode(this.audioContext, 'audio-worklet-processor', 
      { 
        outputChannelCount: [2],
        processorOptions: 
        {  
          ringBuffer: this.ringBuffer,
          state: this.state
        } 
      });

      const wsWorkerOpts: WsWorkerOpts = {
        type: "INIT", 
        roomId: this.roomId, 
        userId: this.userId, 
        ringBuffer: this.ringBuffer,
        state: this.state,
      }

    this.wsWorker.postMessage(wsWorkerOpts);
  }

  public stopListening() {
    if (this.wsWorker === undefined) throw new Error("Ws worker wasnt initialized");
    if (this.audioContext === undefined) throw new Error("Audio context wasnt initialized");
    this.wsWorker.terminate();
    this.audioContext.close();
  }

  private onPostMessage = async (messageEvent: MessageEvent<WsWorkerMessage>) => {
    if (this.audioContext === undefined) throw new Error("Audio context wasnt initialized");
    if (this.audioWorklet === undefined) throw new Error("Audio worklet wasnt initialized");
    if (messageEvent.data.type === 'WS_WORKER_READY') {
      console.info("Ws worker intialized...");

      this.audioWorklet.connect(this.audioContext.destination);

      this.audioContext.resume();
    }
  }
}
