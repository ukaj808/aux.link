import { AuxAudioPlayer, AuxAudioPlayerEvent, StreamStartingEvent } from "../aux-audio-player";
import { MusicStreamerState, RoomView, SongStartingEvent } from "../interface";
import { RestClient } from "../rest-client";
import { RoomMessageListener } from "../room-message-listener";
import { fromConnectingToDisconnected } from "./transitions/from-connecting-to-disconnecting";
import { fromConnectingToNotRunning } from "./transitions/from-connecting-to-not-running";
import { fromConnectingToStreaming } from "./transitions/from-connecting-to-streaming";
import { fromDisconnectedToConnecting } from "./transitions/from-disconnected-to-connecting";

export type CurrentlyPlayingState = 'Connecting' | 'Disconnected' | MusicStreamerState;

export class CurrentlyPlayingElement {

  private el: HTMLElement;
  private xState: CurrentlyPlayingState;
  private roomMessageListener: RoomMessageListener;
  private restClient: RestClient;
  private auxAudioPlayer: AuxAudioPlayer;
  private analyser: AnalyserNode;
  private listening: boolean;
  private audioCanvas: HTMLCanvasElement;
  private overlayEl: HTMLDivElement;
  private description: HTMLSpanElement;
  private listenIcon: HTMLElement;
  private disconnectBtn: HTMLButtonElement
  private countdownTimer: HTMLSpanElement;
  private loadingBars: HTMLDivElement;
  private canvasCtx: CanvasRenderingContext2D;
  private buffer: Float32Array;
  private drawVisual?: number;

  constructor(roomMessageListener: RoomMessageListener, restClient: RestClient, auxAudioPlayer: AuxAudioPlayer, analyser: AnalyserNode) {
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

    const disconnectBtn = document.getElementById("cp-disconnect-btn");
    if (!disconnectBtn) throw new Error('No disconnect button element found');
    this.disconnectBtn = disconnectBtn as HTMLButtonElement;
    this.disconnectBtn.addEventListener("click", () => this.transitionTo('Disconnected'));

    const countdownTimer = document.getElementById("cp-timer");
    if (!countdownTimer) throw new Error('No countdown timer element found');
    this.countdownTimer = countdownTimer as HTMLSpanElement;

    const loadingBars = document.getElementById("cp-loading");
    if (!loadingBars) throw new Error('No loading bars element found');
    this.loadingBars = loadingBars as HTMLDivElement;

    const description = document.getElementById("cp-desc");
    if (!description) throw new Error('No description element found');
    this.description = description as HTMLSpanElement;

    this.roomMessageListener = roomMessageListener;

    this.roomMessageListener.subscribe('SongStartingEvent', (data) => {
      const songStartingEvent = data as SongStartingEvent;
      if (songStartingEvent.s === 5) {
        this.transitionTo('Countdown');
      } else if (songStartingEvent.s === 0) {
        this.transitionTo('Polling');
      }
      this.countdownTimer.innerHTML = songStartingEvent.s.toString();
    });

    this.xState = 'Disconnected';

    this.listening = false;
    this.restClient = restClient;
    this.auxAudioPlayer = auxAudioPlayer;
    this.analyser = analyser;
    this.analyser.fftSize = 256;
    this.buffer = new Float32Array(this.analyser.frequencyBinCount);

    this.el.addEventListener("click", () => this.transitionTo('Connecting'));
    this.disconnectBtn.addEventListener("click", () => this.transitionTo('Disconnected'));
  }

  private transitionTo(targetState: CurrentlyPlayingState, data?: any) {
    switch (targetState) {
      case 'Disconnected':
        if (this.xState === 'Connecting') { 
          fromConnectingToDisconnected(this.auxAudioPlayer, this.overlayEl, this.disconnectBtn, this.listening);
        }
        break;
      case 'Connecting':
        if (this.xState === 'Disconnected') {
          fromDisconnectedToConnecting(this.xState, this.restClient, this.auxAudioPlayer, this.overlayEl, this.loadingBars, this.disconnectBtn, this.listening, this.transitionTo.bind(this));
        } 
        break;
      case 'NotRunning':
        if (this.xState === 'Connecting') {
          fromConnectingToNotRunning(this.description);
        }
      case 'Streaming':
        if (this.xState === 'Connecting') {
          fromConnectingToStreaming(this.description, data);
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
