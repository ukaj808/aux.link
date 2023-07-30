import { AuxAudioPlayer, AuxAudioPlayerEvent, StreamStartingEvent } from "../aux-audio-player";
import { MusicStreamerState, RoomMessage, RoomView, SongStartingEvent, SongUploadedEvent } from "../interface";
import { RestClient } from "../rest-client";
import { RoomMessageListener } from "../room-message-listener";
import { AudioVisualizer } from "./audio-visualizer";
import { fromConnectingToCountdown } from "./transitions/from-connecting/from-connecting-to-countdown";
import { fromConnectingToDisconnected } from "./transitions/from-connecting/from-connecting-to-disconnecting";
import { fromConnectingToNotRunning } from "./transitions/from-connecting/from-connecting-to-not-running";
import { fromConnectingToPolling } from "./transitions/from-connecting/from-connecting-to-polling";
import { fromConnectingToStreaming } from "./transitions/from-connecting/from-connecting-to-streaming";
import { fromCountdownToDisconnected } from "./transitions/from-countdown/from-countdown-to-disconnected";
import { fromCountdownToPolling } from "./transitions/from-countdown/from-countdown-to-polling";
import { fromDisconnectedToConnecting } from "./transitions/from-disconnected-to-connecting";
import { fromNotRunningToCountdown } from "./transitions/from-notrunning/from-notrunning-to-countdown";
import { fromNotRunningToDisconnected } from "./transitions/from-notrunning/from-notrunning-to-disconnected";
import { fromPollingToCountdown } from "./transitions/from-polling/from-polling-to-countdown";
import { fromPollingToDisconnected } from "./transitions/from-polling/from-polling-to-disconnected";
import { fromPollingToStreaming } from "./transitions/from-polling/from-polling-to-streaming";
import { fromStreamingToCountdown } from "./transitions/from-streaming/from-streaming-to-countdown";
import { fromStreamingToDisconnected } from "./transitions/from-streaming/from-streaming-to-disconnected";
import { createMachine, fromPromise, interpret } from "xstate";

export type CurrentlyPlayingState = 'Connecting' | 'Disconnected' | MusicStreamerState;

export class CurrentlyPlayingElement {

  private machine = createMachine({
    id: 'currently-playing',
    initial: 'Disconnected',
    states: {
      Disconnected: {
        exit: 'removeOverlay',
        on: {
          CONNECT: { target: 'Connecting' },
        },
      },
      Connecting: {
        entry: ['showLoading', 'showDisconnectButton'],
        exit: 'hideLoading',
        on: {
          DISCONNECT: { target: 'Disconnected' },
        },
        invoke: {
          src: 'connect',
          onDone: [
            {
              guard: 'isNotRunning',
              target: 'NotRunning',
            },
            {
              guard: 'isStreaming',
              target: 'Streaming',
            },
            {
              guard: 'isCountingDown',
              target: 'Countdown',
            },
            {
              guard: 'isPolling',
              target: 'Polling',
            },
          ],
          onError: {
            target: 'Disconnected',
            //todo: action
          },
        },
      },
      NotRunning: {
        entry: ['showWaitingForCreator', 'subscribeToSongStartingEvent'],
        exit: 'hideWaitingForCreator',
        on: {
          COUNTDOWN_STARTED:{ target: 'Countdown' }, 
          DISCONNECT: { target: 'Disconnected' },
        },
      },
      Countdown: {
        entry: 'showCountdown',
        exit: ['unsubscribeFromSongStartingEvent', 'hideCountdown'],
        on: {
          COUNTDOWN_FINISHED: 'Polling',
          DISCONNECT: { target: 'Disconnected' },
        },
      },
      Polling: {
        entry: ['subscribeToSongUploadedEvent', 'showLoading'],
        exit: ['unsubscribeFromSongUploadedEvent', 'hideLoading'],
        on: {
          SONG_UPLOADED: { target: 'Streaming' },
          TIMEOUT: { target: 'Countdown' },
          DISCONNECT: { target: 'Disconnected' },
        },
      },
      Streaming: {
        entry: ['subscribeToSongStartingEvent', 'showSongTitle', 'startVisualizer'],
        exit: ['hideSongTitle', 'stopVisualizer'],
        on: {
          SONG_FINISHED: 'Countdown', 
          DISCONNECT: 'Disconnected',
        },
      },
    },
  }, 
  {
    actors: {
      connect: fromPromise(() => {
        this.auxAudioPlayer.startListening();
        return this.restClient.getRoom();
      }),
    },
    actions: {
      removeOverlay: () => {
        this.overlayEl.removeEventListener("click", this.overlayClickHandler);
        this.overlayEl.classList.add('invisible');
      },
      showLoading: () => {
        this.loadingBars.classList.remove('hidden');
      },
      hideLoading: () => {
        this.loadingBars.classList.add('hidden');
      }, 
      showCountdown: () => {
        this.countdownTimer.classList.remove('hidden');
      },
      hideCountdown: () => {
        this.countdownTimer.classList.add('hidden');
      },
      showDisconnectButton: () => {
        this.disconnectBtn.classList.remove('hidden');
      },
      hideDisconnectButton: () => {
        this.disconnectBtn.classList.add('hidden');
      },
      showSongTitle: ({context}) => {
        console.log('showStreaming', context);
        this.description.classList.remove('hidden');
        this.description.innerText = "Streaming...";
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
      subscribeToSongUploadedEvent: () => {
        this.roomMessageListener.subscribe('SongUploadedEvent', this.songUploadedEventHandler);
      },
      unsubscribeFromSongUploadedEvent: () => {
        this.roomMessageListener.unsubscribe('SongUploadedEvent', this.songUploadedEventHandler);
      },
      subscribeToSongStartingEvent: () => {
        this.roomMessageListener.subscribe('SongStartingEvent', this.songStartingEventHandler);
      },
      unsubscribeFromSongStartingEvent: () => {
        this.roomMessageListener.unsubscribe('SongStartingEvent', this.songStartingEventHandler);
      },
    },
    guards: {
      isNotRunning: ({event}) => {
        return event.output.currentlyPlayingView.musicState === 'NotRunning';
      },
      isStreaming: ({event}) => {
        return event.output.currentlyPlayingView.musicState === 'Streaming';
      },
      isCountingDown: ({event}) => {
        return event.output.currentlyPlayingView.musicState === 'Countdown';
      },
      isPolling: ({event}) => {
        return event.output.currentlyPlayingView.musicState === 'Polling';
      },
    },
  });

  private actor = interpret(this.machine);
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
  private transitionHistory: CurrentlyPlayingState[];

  private overlayClickHandler = () => 
  {
    console.log(this.actor);
    this.actor.send({ type: 'CONNECT' })
  };

  private songStartingEventHandler: (roomEvent: RoomMessage) => void = (data) => {
      const songStartingEvent = data as SongStartingEvent;  
      this.countdownTimer.innerText = songStartingEvent.s.toString();
      if (songStartingEvent.s === 5) {
        this.actor.send({ type: 'COUNTDOWN_STARTED' });
      } else if (songStartingEvent.s === 0) {
        this.actor.send({ type: 'COUNTDOWN_FINISHED' });
      }
  }

  private songUploadedEventHandler: (roomEvent: RoomMessage) => void = (data) => {
    const songUploadedEvent = data as SongUploadedEvent;
    this.actor.send({ type: 'SONG_UPLOADED' },{ songUploadedEvent });
  }
  

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
    this.overlayEl.addEventListener("click", this.overlayClickHandler);

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

    this.auxAudioPlayer = auxAudioPlayer;

    this.xState = 'Disconnected';
    this.transitionHistory = ['Disconnected'];
    this.audioVisualizer = new AudioVisualizer(analyser, audioCanvas);

    this.listening = false;
    this.restClient = restClient;
    this.analyser = analyser;
    this.analyser.fftSize = 256;

    this.actor.subscribe((state) => {
      console.log(state);
    });
    this.actor.start();

  }

  private transitionTo(targetState: CurrentlyPlayingState, data?: any) {
    switch (targetState) {
      case 'Disconnected':
        if (this.xState === 'Connecting') { 
          fromConnectingToDisconnected(this.roomMessageListener, this.auxAudioPlayer, this.overlayClickHandler, this.songStartingEventHandler, this.songUploadedEventHandler, this.overlayEl,  this.loadingBars, this.disconnectBtn, this.listening);
        } else if (this.xState === 'Streaming') {
          fromStreamingToDisconnected(this.roomMessageListener, this.auxAudioPlayer, this.audioVisualizer, this.overlayEl, this.overlayClickHandler, this.songStartingEventHandler, this.songUploadedEventHandler, this.description, this.disconnectBtn, this.listening);
        } else if (this.xState === 'Polling') {
          fromPollingToDisconnected(this.roomMessageListener, this.auxAudioPlayer, this.overlayEl, this.overlayClickHandler, this.songStartingEventHandler, this.songUploadedEventHandler, this.loadingBars, this.disconnectBtn, this.listening);
        } else if (this.xState === 'Countdown') {
          fromCountdownToDisconnected(this.roomMessageListener, this.auxAudioPlayer, this.overlayEl, this.overlayClickHandler, this.songStartingEventHandler, this.songUploadedEventHandler, this.countdownTimer, this.disconnectBtn, this.listening);
        } else if (this.xState === 'NotRunning') {
          fromNotRunningToDisconnected(this.roomMessageListener, this.auxAudioPlayer, this.overlayEl, this.overlayClickHandler, this.songStartingEventHandler, this.songUploadedEventHandler, this.description, this.disconnectBtn, this.listening);
        } else throw new Error(`Unknown transition from ${this.xState} to ${targetState}`);
        break;
      case 'Connecting':
        if (this.xState === 'Disconnected') {
          fromDisconnectedToConnecting(this.restClient, this.auxAudioPlayer, this.roomMessageListener, this.overlayEl, this.overlayClickHandler, this.songStartingEventHandler, this.songUploadedEventHandler, this.loadingBars, this.disconnectBtn, this.listening, this.transitionTo.bind(this));
        } else throw new Error(`Unknown transition from ${this.xState} to ${targetState}`); 
        break;
      case 'NotRunning':
        if (this.xState === 'Connecting') {
          fromConnectingToNotRunning(this.roomMessageListener, this.songStartingEventHandler, this.description);
        } else throw new Error(`Unknown transition from ${this.xState} to ${targetState}`);
        break;
      case 'Streaming':
        if (this.xState === 'Connecting') {
          fromConnectingToStreaming(this.description, this.audioVisualizer, data);
        } else if (this.xState === 'Polling') {
          console.log('polling to streaming', data);
          fromPollingToStreaming(this.loadingBars, this.description, this.audioVisualizer, data);
        } else throw new Error(`Unknown transition from ${this.xState} to ${targetState}`);
        break;
      case 'Countdown':
        if (this.xState === 'Connecting') {
          fromConnectingToCountdown(this.countdownTimer, this.roomMessageListener, this.transitionTo.bind(this));
        }
        else if (this.xState === 'Streaming') {
          fromStreamingToCountdown(this.audioVisualizer, this.countdownTimer, this.description, this.roomMessageListener, this.transitionTo.bind(this));
        } else if (this.xState === 'Polling') {
          fromPollingToCountdown(this.loadingBars, this.countdownTimer, this.roomMessageListener, this.transitionTo.bind(this));
        } else if (this.xState === 'NotRunning') {
          fromNotRunningToCountdown(this.description, this.countdownTimer, this.roomMessageListener, this.transitionTo.bind(this));
        } else throw new Error(`Unknown transition from ${this.xState} to ${targetState}`);
        break;
      case 'Polling':
        if (this.xState === 'Connecting') {
          fromConnectingToPolling(this.loadingBars);
        } else if (this.xState === 'Countdown') {
          fromCountdownToPolling(this.countdownTimer, this.loadingBars);
        } else throw new Error(`Unknown transition from ${this.xState} to ${targetState}`);
        break;
    }
    this.xState = targetState;
    this.transitionHistory.push(targetState);
    console.info(this.transitionHistory);
  }

}
