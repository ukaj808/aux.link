import { AuxAudioPlayer, AuxAudioPlayerEvent, StreamStartingEvent } from "./aux-audio-player";
import { RoomMessageListener } from "./room-message-listener";

type MusicStreamerState = 'Streaming' | 'Countdown' | 'Polling' | 'NotRunning';
type CurrentlyPlayingView = {
    musicStreamerState: MusicStreamerState;
    currentlyPlayingSong: string | null;
}
type CurrentlyPlayingState = 'Connecting' | 'Disconnected' | MusicStreamerState;

export class CurrentlyPlayingElement {

  private el: HTMLElement;
  private iState: CurrentlyPlayingView;
  private xState: CurrentlyPlayingState;
  private roomMessageListener: RoomMessageListener;
  private auxAudioPlayer: AuxAudioPlayer;
  private analyser: AnalyserNode;
  private listening: boolean;
  private audioCanvas: HTMLCanvasElement;
  private overlayEl: HTMLDivElement
  private listenIcon: HTMLElement;
  private countdownTimer: HTMLSpanElement;
  private loadingBars: HTMLDivElement;
  private canvasCtx: CanvasRenderingContext2D;
  private buffer: Float32Array;
  private drawVisual?: number;

  constructor(roomMessageListener: RoomMessageListener, auxAudioPlayer: AuxAudioPlayer, analyser: AnalyserNode) {
    const optEl = document.getElementById("currently-playing");
    if (!optEl) throw new Error('No currently playing element found');
    this.el = optEl;

    const stateAttribute = optEl.getAttribute('data-state');
    if (!stateAttribute) throw new Error('No state attribute found');
    this.iState = JSON.parse(stateAttribute) as CurrentlyPlayingView;

    const audioCanvas = document.getElementById("audio-visualizer") as HTMLCanvasElement;
    if (!audioCanvas) throw new Error('No audio canvas element found');
    this.audioCanvas = audioCanvas;

    const canvasCtx = this.audioCanvas.getContext("2d");
    if (!canvasCtx) throw new Error('No canvas context found');
    this.canvasCtx = canvasCtx;
    this.canvasCtx.clearRect(0, 0, this.audioCanvas.width, this.audioCanvas.height);

    const overlayEl = document.getElementById("cp-overlay");
    if (!overlayEl) throw new Error('No overlay element found');
    this.overlayEl = overlayEl as HTMLDivElement;

    const listenIcon = document.getElementById("listen-icon");
    if (!listenIcon) throw new Error('No listen icon element found');
    this.listenIcon = listenIcon;
    
    const countdownTimer = document.getElementById("countdown-timer");
    if (!countdownTimer) throw new Error('No countdown timer element found');
    this.countdownTimer = countdownTimer as HTMLSpanElement;

    const loadingBars = document.getElementById("cp-loading");
    if (!loadingBars) throw new Error('No loading bars element found');
    this.loadingBars = loadingBars as HTMLDivElement;

    this.roomMessageListener = roomMessageListener;
    this.roomMessageListener.subscribe('SongStartingEvent', (data) => {
      const songStartingEvent = data as SongStartingEvent;
      if (songStartingEvent.s === 5) {
        this.transitionTo('countdown');
      } else if (songStartingEvent.s === 0) {
        this.transitionTo('loading');
      }
      this.countdownTimer.innerHTML = songStartingEvent.s.toString();
    });
    this.listening = false;
    this.auxAudioPlayer = auxAudioPlayer;
    this.analyser = analyser;
    this.analyser.fftSize = 256;
    this.buffer = new Float32Array(this.analyser.frequencyBinCount);

    this.el.addEventListener("click", () => this.onSectionClick());
  }

  private transitionTo(targetState: CurrentlyPlayingState) {
    switch (targetState) {
      case 'Disconnected':
        // Should all states can transition to Disconnected...
        // The user should be able to disconnect at anytime
      case 'Connecting':
        if (this.xState === 'Disconnected') {
          // fromDisconnectedToConnecting
        } else {
          throw Error(`Invalid transition from ${this.xState} to ${targetState}`);
        }
        break;
      case 'NotRunning':
        if (this.xState === 'Connecting') {
          // fromConnectingToNotRunning
        } else { 
          throw Error(`Invalid transition from ${this.xState} to ${targetState}`); 
        }
        break;
      case 'Streaming':
        if (this.xState === 'Connecting') {
          // fromConnectingToStreaming
        } else if (this.xState === 'Polling') {
          // fromPolllingToStreaming
        }
        break;
      case 'Countdown':
        if (this.xState === 'Connecting') {
          // fromConnectingToCountdown
        }
        else if (this.xState === 'Streaming') {
          // fromStreamingToCountdown
        } else if (this.xState === 'Polling') {
          // when the user didnt have a song to upload.
          // fromPollingToCountdown
        }
        break;
      case 'Polling':
        if (this.xState === 'Connecting') {
          // fromConnectingToPolling
        } else if (this.xState === 'Countdown') {
          // fromCountdownToPolling
        }
        break;
    }
    this.xState = targetState;
  }
  
  private fromDisconnectedToConnecting() {
    const onStreamConnected = (data: AuxAudioPlayerEvent) => {
      const streamStartingEvent = data as StreamStartingEvent;
      this.auxAudioPlayer.unsubscribe('STREAM_STARTING', onStreamConnected);
      this.toggleLoading();
      // route logic??
    }
    this.auxAudioPlayer.subscribe('STREAM_CONNECTED', onStreamConnected);
    this.toggleOverlay();
    this.toggleLoading();
    this.auxAudioPlayer.startListening();
    this.listening = true;
  }
  
  private onSectionClick() {
    if (!this.listening) this.transitionTo('loading');
    else this.transitionTo('disconnected');
  }

  private toggleOverlay() {
    this.el.classList.toggle("overlay");
  }
  
  private toggleCountdown() {
    this.countdownTimer.classList.toggle("hidden");
  }

  private toggleLoading() {
    this.loadingBars.classList.toggle("hidden");
  }

  private draw() {
    this.drawVisual = requestAnimationFrame(this.draw.bind(this));
    this.analyser.getFloatFrequencyData(this.buffer);
    this.canvasCtx.fillStyle = 'rgb(48, 49, 53)';
    this.canvasCtx.fillRect(0, 0, this.audioCanvas.width, this.audioCanvas.height);

    const barWidth = (this.audioCanvas.width / this.analyser.frequencyBinCount) * 2.5;
    let barHeight;

    let x = 0;

    for (let i = 0; i < this.analyser.frequencyBinCount; i++) {
      barHeight = this.buffer[i];

      this.canvasCtx.fillStyle = `rgb(${barHeight + 100}, 50, 50)`;
      this.canvasCtx.fillRect(x, this.audioCanvas.height - barHeight / 2, barWidth, barHeight);

      x += barWidth + 1;
    }
  }

}
