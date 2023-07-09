import { AuxAudioPlayer } from "./aux-audio-player";
import { RoomMessageListener } from "./room-message-listener";

export class CurrentlyPlayingElement {

  private el: HTMLElement;
  private state: 'playing' | 'countdown' | 'loading' | 'not_started' | 'disconnected';
  private roomMessageListener: RoomMessageListener;
  private auxAudioPlayer: AuxAudioPlayer;
  private analyser: AnalyserNode;
  private listening: boolean;
  private audioCanvas: HTMLCanvasElement;
  private overlayEl: HTMLDivElement;
  private listenIcon: HTMLElement;
  private countdownTimer: HTMLSpanElement;
  private canvasCtx: CanvasRenderingContext2D;
  private buffer: Float32Array;
  private drawVisual?: number;

  constructor(roomMessageListener: RoomMessageListener, auxAudioPlayer: AuxAudioPlayer, analyser: AnalyserNode) {
    const optEl = document.getElementById("currently-playing");
    if (!optEl) throw new Error('No currently playing element found');
    this.el = optEl;

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

    this.state = 'disconnected';

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

  private transitionTo(state: 'playing' | 'countdown' | 'loading' | 'not_started' | 'disconnected') {
    switch (state) {
      case 'playing':
        if (this.state === 'countdown') {
        } else if (this.state === 'loading') {
        } else throw Error(`Invalid transition from ${this.state} to ${state}`);
        break;
      case 'countdown':
        if (this.state === 'playing') {
        } else if (this.state === 'loading') {
          this.loadingToCountdown();
        } else if (this.state === 'not_started') {
        } else if (this.state === 'disconnected') {
        } else throw Error(`Invalid transition from ${this.state} to ${state}`);
        break;
      case 'loading':
        if (this.state === 'playing') {
        } else if (this.state === 'countdown') {
          this.countdownToLoading();
        } else if (this.state === 'not_started') {
        } else if (this.state === 'disconnected') {
          this.disconnectedToLoading();
        } else throw Error(`Invalid transition from ${this.state} to ${state}`);
        break;
      case 'not_started':
        if (this.state === 'playing') {
        } else if (this.state === 'countdown') {
        } else if (this.state === 'loading') {
        } else if (this.state === 'disconnected') {
        } else throw Error(`Invalid transition from ${this.state} to ${state}`);
        break;
      case 'disconnected':
        if (this.state === 'playing') {
          this.playingToDisconnected();
        } else if (this.state === 'countdown') {
          this.countdownToDisconnected();
        } else if (this.state === 'loading') {
          this.loadingToDisconnected();
        } else if (this.state === 'not_started') {
          this.notStartedToDisconnected();
        } else throw Error(`Invalid transition from ${this.state} to ${state}`);
        break;
    }
    this.state = state;
  }
  
  private countdownToLoading() {
    this.toggleCountdown();
  }
  
  private loadingToCountdown() {
    this.toggleCountdown();
  }

  private playingToDisconnected() {
    this.auxAudioPlayer.stopListening();
    this.listening = false;
    this.toggleOverlay();
  }

  private loadingToDisconnected() {
    this.auxAudioPlayer.stopListening();
    this.listening = false;
    this.toggleOverlay();
  }

  private countdownToDisconnected() {
    this.auxAudioPlayer.stopListening();
    this.listening = false;
    this.toggleOverlay();
  }

  private notStartedToDisconnected() {
    this.auxAudioPlayer.stopListening();
    this.listening = false;
    this.toggleOverlay();
  }

  private disconnectedToLoading() {
    this.auxAudioPlayer.startListening();
    this.listening = true;
    this.toggleOverlay();
    this.draw();
  }
  
  private onSectionClick() {
    if (!this.listening) this.transitionTo('loading');
    else this.transitionTo('disconnected');
  }

  private toggleOverlay() {
    this.el.classList.toggle("overlay");
  }
  
  private toggleCountdown() {
    this.el.classList.toggle("overlay");
    this.countdownTimer.classList.toggle("hidden");
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
