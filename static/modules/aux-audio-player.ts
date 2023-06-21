export class AuxAudioPlayer {
  private roomId: string;
  private userId?: string;
  private audioContext?: AudioContext;
  private wsWorker?: Worker;
  private audioWorklet?: AudioWorkletNode;
  private ringBufferSize?: number;
  private ringBuffer?: SharedArrayBuffer;
  private state?: SharedArrayBuffer;
  private readerOffset?: SharedArrayBuffer;
  private samplesRead?: SharedArrayBuffer;
  private writerOffset?: SharedArrayBuffer;
  private samplesWritten?: SharedArrayBuffer;
  private break?: SharedArrayBuffer;

  constructor(roomId: string) {
    this.roomId = roomId;
    this.ringBufferSize = 262144; //8 kb
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
    this.readerOffset = new SharedArrayBuffer(4);
    this.samplesRead = new SharedArrayBuffer(4);
    this.writerOffset = new SharedArrayBuffer(4);
    this.samplesWritten = new SharedArrayBuffer(4);
    this.break = new SharedArrayBuffer(4);

    this.wsWorker = new Worker('public/audio_socket_worker_bundle.js');
    this.wsWorker.onmessage = this.onWsWorkerEvent;

    await this.audioContext.audioWorklet.addModule('public/audio_worklet_processor_bundle.js');

    this.audioWorklet = new AudioWorkletNode(this.audioContext, 'audio-worklet-processor', 
      { 
        outputChannelCount: [2],
        processorOptions: 
        {  
          ringBuffer: this.ringBuffer,
          state: this.state,
          readerOffset: this.readerOffset,
          writerOffset: this.writerOffset,
          samplesRead: this.samplesRead,
          samplesWritten: this.samplesWritten,
          break: this.break,
        } 
      });

    this.audioWorklet.port.onmessage = this.onAudioWorkletEvent;

      const wsWorkerOpts: WsWorkerOpts = {
        type: "INIT", 
        roomId: this.roomId, 
        userId: this.userId, 
        ringBuffer: this.ringBuffer,
        state: this.state,
        readerOffset: this.readerOffset,
        samplesRead: this.samplesRead,
        writerOffset: this.writerOffset,
        samplesWritten: this.samplesWritten,
        break: this.break,
      }

    this.wsWorker.postMessage(wsWorkerOpts);
  }

  public stopListening() {
    if (this.wsWorker === undefined) throw new Error("Ws worker wasnt initialized");
    if (this.audioContext === undefined) throw new Error("Audio context wasnt initialized");
    if (this.audioWorklet === undefined) throw new Error("Audio worklet wasnt initialized");
    this.wsWorker.terminate();
    this.wsWorker = undefined;
    this.audioWorklet.disconnect(this.audioContext.destination);
    this.audioWorklet = undefined;
    this.audioContext.close();
    this.audioContext = undefined;
  }

  private onWsWorkerEvent = async (event: MessageEvent<WsWorkerEvent>) => {
    if (this.audioContext === undefined) throw new Error("Audio context wasnt initialized");
    if (this.audioWorklet === undefined) throw new Error("Audio worklet wasnt initialized");
    switch (event.data.type) {
      case 'WS_WORKER_READY': {
        console.info("Ws worker intialized...");
        this.audioWorklet.connect(this.audioContext.destination);
        this.audioContext.resume();
        break;
      }
      case 'WRITE_SONG_STARTED': {
        this.audioWorklet.port.postMessage({ type: event.data.type });
        this.audioContext.resume();
        break;
      }
      case 'WRITE_SONG_FINISHED': {
        this.audioWorklet.port.postMessage({ type: event.data.type, offset: event.data.offset });
        break;
      }
    }
  }

  private onAudioWorkletEvent = (event: MessageEvent<AudioWorkletEvent>) => {
    switch (event.data.type) {
      case 'READ_SONG_FINISHED':
        // this.audioContext?.suspend();
        break;
    }
  }
}
