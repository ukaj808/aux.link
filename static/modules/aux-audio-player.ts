export class AuxAudioPlayer {
  private roomId: string;
  private userId: string | null;
  private audioContext: AudioContext;
  private wsWorker: Worker;
  private audioWorklet: AudioWorkletNode | null;
  private ringBuffer: SharedArrayBuffer;
  private state: SharedArrayBuffer;


  constructor(roomId: string) {
    const ringBufferSize = 1920000; // 5 Seconds of audio @ 384000 bytes per second
    this.ringBuffer = new SharedArrayBuffer(ringBufferSize);
    this.state = new SharedArrayBuffer(1);

    this.roomId = roomId;
    this.userId = null;
    this.audioContext = new AudioContext(
      {
        latencyHint: "playback", 
        sampleRate: 48000,
      }
    );
    this.audioContext.suspend();
    this.wsWorker = new Worker('public/audio_socket_worker_bundle.js');
    this.wsWorker.onmessage = this.onPostMessage;
    this.audioWorklet = null;
  }

  public async startListening(userId: string) {

    this.userId = userId;

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
    this.wsWorker.terminate();
  }

  private onPostMessage = async (messageEvent: MessageEvent<WsWorkerMessage>) => {
      if (messageEvent.data.type === 'WS_WORKER_READY') {
        console.info("Ws worker intialized...");

        this.audioWorklet?.connect(this.audioContext.destination);

        this.audioContext.resume();
      }
    }

}
