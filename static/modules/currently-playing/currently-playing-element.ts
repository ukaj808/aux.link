import { AuxAudioPlayer, AuxAudioPlayerEvent, StreamStartingEvent } from "../aux-audio-player";
import { CurrentlyPlayingView, MusicStreamerState, RoomMessage, RoomView, SongStartingEvent, SongUploadTimeoutEvent, SongUploadedEvent } from "../interface";
import { RestClient } from "../rest-client";
import { RoomMessageListener } from "../room-message-listener";
import { AudioVisualizer } from "./audio-visualizer";
import { createMachine, fromPromise, interpret } from "xstate";

export class CurrentlyPlayingElement {

  private el: HTMLElement;
  private canvasMachine: any;
  private listenMachine: any;
  private roomMessageListener: RoomMessageListener;
  private restClient: RestClient;
  private auxAudioPlayer: AuxAudioPlayer;
  private audioVisualizer: AudioVisualizer;
  private analyser: AnalyserNode;
  private listening: boolean;
  private audioCanvas: HTMLCanvasElement;
  private overlayEl: HTMLDivElement;
  private description: HTMLSpanElement;
  private connectBtn: HTMLButtonElement
  private countdownTimer: HTMLSpanElement;
  private loadingBars: HTMLDivElement;
  private initialState: MusicStreamerState;

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

    const connectBtn = document.getElementById("cp-connect-btn");
    if (!connectBtn) throw new Error('No disconnect button element found');
    this.connectBtn = connectBtn as HTMLButtonElement;

    const countdownTimer = document.getElementById("cp-timer");
    if (!countdownTimer) throw new Error('No countdown timer element found');
    this.countdownTimer = countdownTimer as HTMLSpanElement;

    const loadingBars = document.getElementById("cp-loading");
    if (!loadingBars) throw new Error('No loading bars element found');
    this.loadingBars = loadingBars as HTMLDivElement;

    const description = document.getElementById("cp-desc");
    if (!description) throw new Error('No description element found');
    this.description = description as HTMLSpanElement;

    const state = optEl.getAttribute('data-state');
    if (!state) throw new Error('No initial state found');
    this.initialState = state as MusicStreamerState;

    this.roomMessageListener = roomMessageListener;

    this.auxAudioPlayer = auxAudioPlayer;

    this.audioVisualizer = new AudioVisualizer(analyser, audioCanvas);

    this.listening = false;
    this.restClient = restClient;
    this.analyser = analyser;
    this.analyser.fftSize = 256;


    this.canvasMachine = createMachine({
    id: 'canvas',
    initial: this.initialState,
    states: {
      NotRunning: {
        entry: ['showWaitingForCreator'],
        exit: 'hideWaitingForCreator',
        on: {
          COUNTDOWN_STARTED:{ target: 'Countdown' }, 
        },
      },
      Countdown: {
        entry: ['showCountdown'],
        exit: ['hideCountdown'],
        on: {
          COUNTDOWN_FINISHED: 'Polling',
        },
      },
      Polling: {
        entry: ['showLoading'],
        exit: ['hideLoading'],
        on: {
          SONG_UPLOADED: { target: 'Streaming' },
          UPLOAD_TIMEOUT: { target: 'Countdown' },
        },
      },
      Streaming: {
        entry: ['showSongTitle', 'startVisualizer'],
        exit: ['hideSongTitle', 'stopVisualizer'],
        on: {
          SONG_FINISHED: 'Countdown', 
        },
      },
    },
  }, 
  {
    actions: {
      connect: () => {
        this.auxAudioPlayer.startListening();
      },
      disconnect: () => {
        this.auxAudioPlayer.stopListening();
      },
      removeOverlay: () => {
        this.overlayEl.classList.add('invisible');
      },
      showOverlay: () => {
        this.overlayEl.classList.remove('invisible');
      },
      showLoading: () => {
        this.loadingBars.classList.remove('hidden');
      },
      hideLoading: () => {
        this.loadingBars.classList.add('hidden');
      }, 
      showCountdown: ({event}) => {
        if (event.output) {
          const roomView = event.output as RoomView;
          if (roomView.currentlyPlayingView.countdown) {
            this.countdownTimer.innerText = roomView.currentlyPlayingView.countdown.toString();
          }
        }
        this.countdownTimer.classList.remove('hidden');
      },
      hideCountdown: () => {
        this.countdownTimer.classList.add('hidden');
      },
      showSongTitle: ({event}) => {
        if (event.output) {
          const roomView = event.output as RoomView;
          if (roomView.currentlyPlayingView.song) {
            this.description.innerText = roomView.currentlyPlayingView.song;
          }
        }
        this.description.classList.remove('hidden');
      },
      hideSongTitle: () => {
        this.description.classList.add('hidden');
        this.description.innerText = "";
      },
      startVisualizer: () => {
        this.audioVisualizer.start();
      },
      stopVisualizer: () => {
        this.audioVisualizer.stop();
      },
      showWaitingForCreator: () => {
        this.description.classList.remove("hidden");
        this.description.innerText = "Waiting for the creator to start the music...";
      },
      hideWaitingForCreator: () => {
        this.description.classList.add("hidden");
        this.description.innerText = "";
      },
    },
  });

  this.listenMachine = createMachine({
    id: 'listen',
    initial: 'Disconnected',
    states: {
      Disconnected: {
        entry: ['disconnect'],
        on: {
          CONNECT: 'Connected',
        },
      },
      Connected: {
        entry: ['connect'],
        on: {
          DISCONNECT: 'Disconnected',
        },
      },
    },
  },
  {
    actions: {
      connect: () => {
        this.auxAudioPlayer.startListening();
        // remove event listener; change button
      },
      disconnect: () => {
        this.auxAudioPlayer.stopListening();
        // remove add listener; change button
      },  
    },
  });
  
    const canvasActor = interpret(this.canvasMachine);
    const listenActor = interpret(this.listenMachine);

    this.roomMessageListener.subscribe('SongUploadTimeoutEvent', (roomEvent: RoomMessage) => {
      canvasActor.send({ type: 'UPLOAD_TIMEOUT' });
    });

    this.roomMessageListener.subscribe('SongUploadedEvent', (roomEvent: RoomMessage) => {
      const songUploadedEvent = roomEvent as SongUploadedEvent;
      this.description.innerText = songUploadedEvent.title;
      canvasActor.send({ type: 'SONG_UPLOADED' });
    });

    this.roomMessageListener.subscribe('SongStartingEvent', (roomEvent: RoomMessage) => {
      const songStartingEvent = roomEvent as SongStartingEvent;  
      this.countdownTimer.innerText = songStartingEvent.s.toString();
      if (songStartingEvent.s === 5) {
        canvasActor.send({ type: 'COUNTDOWN_STARTED' });
      } else if (songStartingEvent.s === 0) {
        canvasActor.send({ type: 'COUNTDOWN_FINISHED' });
      }
    });

    canvasActor.subscribe((state) => {
      console.log('Canvas Actor', state);
    });

    listenActor.start();
    canvasActor.start();
  }

}
