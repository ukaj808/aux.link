import { AuxAudioPlayer, AuxAudioPlayerEvent, StreamStartingEvent } from "../aux-audio-player";
import { MusicStreamerState, RoomView, SongStartingEvent } from "../interface";
import { RestClient } from "../rest-client";
import { RoomMessageListener } from "../room-message-listener";
import { AudioVisualizer } from "./audio-visualizer";
import { fromConnectingToCountdown } from "./transitions/from-connecting-to-countdown";
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
  private audioVisualizer: AudioVisualizer;
  private analyser: AnalyserNode;
  private listening: boolean;
  private audioCanvas: HTMLCanvasElement;
  private overlayEl: HTMLDivElement;
  private description: HTMLSpanElement;
  private listenIcon: HTMLElement;
  private disconnectBtn: HTMLButtonElement
  private countdownTimer: HTMLSpanElement;
  private loadingBars: HTMLDivElement;

  constructor(roomMessageListener: RoomMessageListener, restClient: RestClient, auxAudioPlayer: AuxAudioPlayer, analyser: AnalyserNode) {
    const optEl = document.getElementById("currently-playing");
    if (!optEl) throw new Error('No currently playing element found');
    this.el = optEl;

    const audioCanvas = document.getElementById("audio-visualizer") as HTMLCanvasElement;
    if (!audioCanvas) throw new Error('No audio canvas element found');
    this.audioCanvas = audioCanvas;

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
        this.transitionTo('Countdown', this.roomMessageListener);
      } 
    });

    this.xState = 'Disconnected';
    this.audioVisualizer = new AudioVisualizer(analyser, audioCanvas);

    this.listening = false;
    this.restClient = restClient;
    this.auxAudioPlayer = auxAudioPlayer;
    this.analyser = analyser;
    this.analyser.fftSize = 256;

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
          fromConnectingToStreaming(this.description, this.audioVisualizer, data);
        } else if (this.xState === 'Polling') {

        }
        break;
      case 'Countdown':
        if (this.xState === 'Connecting') {
          fromConnectingToCountdown(this.countdownTimer, data);
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

}
