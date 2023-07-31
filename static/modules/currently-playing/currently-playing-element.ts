import { AuxAudioPlayer, AuxAudioPlayerEvent, StreamStartingEvent } from "../aux-audio-player";
import { CurrentlyPlayingView, MusicStreamerState, RoomMessage, RoomView, SongStartingEvent, SongUploadTimeoutEvent, SongUploadedEvent } from "../interface";
import { RestClient } from "../rest-client";
import { RoomMessageListener } from "../room-message-listener";
import { AudioVisualizer } from "./audio-visualizer";
import { createMachine, fromPromise, interpret } from "xstate";

export type CurrentlyPlayingState = 'Connecting' | 'Disconnected' | MusicStreamerState;

export class CurrentlyPlayingElement {

  private el: HTMLElement;
  private roomMessageListener: RoomMessageListener;
  private restClient: RestClient;
  private auxAudioPlayer: AuxAudioPlayer;
  private audioVisualizer: AudioVisualizer;
  private analyser: AnalyserNode;
  private listening: boolean;
  private audioCanvas: HTMLCanvasElement;
  private overlayEl: HTMLDivElement;
  private description: HTMLSpanElement;
  private disconnectBtn: HTMLButtonElement
  private countdownTimer: HTMLSpanElement;
  private loadingBars: HTMLDivElement;
  private initialState: CurrentlyPlayingView;

  private machine = createMachine({
    id: 'currently-playing',
    initial: 'Disconnected',
    states: {
      Disconnected: {
        exit: ['removeOverlay', 'showDisconnectButton', 'unsubscribeFromSongStartingEvent'],
        entry: ['showOverlay', 'hideDisconnectButton', 'disconnect', 'subscribeToSongStartingEvent', 'showCountdown'],
        on: {
          CONNECT: { target: 'Connecting' },
        },
      },
      Connecting: {
        entry: ['showLoading'],
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
        entry: ['showCountdown'],
        exit: ['hideCountdown'],
        on: {
          COUNTDOWN_FINISHED: 'Polling',
          DISCONNECT: { target: 'Disconnected' },
        },
      },
      Polling: {
        entry: ['subscribeToSongUploadTimeoutEvent', 'subscribeToSongUploadedEvent', 'showLoading'],
        exit: ['unsubscribeFromSongUploadTimeoutEvent', 'unsubscribeFromSongUploadedEvent', 'hideLoading'],
        on: {
          SONG_UPLOADED: { target: 'Streaming' },
          UPLOAD_TIMEOUT: { target: 'Countdown' },
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
      disconnect: () => {
        this.auxAudioPlayer.stopListening();
      },
      removeOverlay: () => {
        this.overlayEl.removeEventListener("click", this.overlayClickHandler);
        this.overlayEl.classList.add('invisible');
      },
      showOverlay: () => {
        this.overlayEl.addEventListener("click", this.overlayClickHandler);
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
      showDisconnectButton: () => {
        this.disconnectBtn.classList.remove('hidden');
        this.disconnectBtn.addEventListener('click', this.disconnectButtonClickHandler);
      },
      hideDisconnectButton: () => {
        this.disconnectBtn.classList.add('hidden');
        this.disconnectBtn.removeEventListener('click', this.disconnectButtonClickHandler);
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
      subscribeToSongUploadTimeoutEvent: () => {
        this.roomMessageListener.subscribe('SongUploadTimeoutEvent', this.songUploadTimeoutEventHandler);
      },
      unsubscribeFromSongUploadTimeoutEvent: () => {
        this.roomMessageListener.unsubscribe('SongUploadTimeoutEvent', this.songUploadTimeoutEventHandler);
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

  private overlayClickHandler = () => 
  {
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
    this.description.innerText = songUploadedEvent.title;
    this.actor.send({ type: 'SONG_UPLOADED' });
  }

  private songUploadTimeoutEventHandler: (roomEvent: RoomMessage) => void = () => {
    this.actor.send({ type: 'UPLOAD_TIMEOUT' });
  }

  private disconnectButtonClickHandler = () => {
    this.actor.send({ type: 'DISCONNECT' });
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

    const disconnectBtn = document.getElementById("cp-disconnect-btn");
    if (!disconnectBtn) throw new Error('No disconnect button element found');
    this.disconnectBtn = disconnectBtn as HTMLButtonElement;

    const countdownTimer = document.getElementById("cp-timer");
    if (!countdownTimer) throw new Error('No countdown timer element found');
    this.countdownTimer = countdownTimer as HTMLSpanElement;

    const loadingBars = document.getElementById("cp-loading");
    if (!loadingBars) throw new Error('No loading bars element found');
    this.loadingBars = loadingBars as HTMLDivElement;

    const description = document.getElementById("cp-desc");
    if (!description) throw new Error('No description element found');
    this.description = description as HTMLSpanElement;

    const initialState = optEl.getAttribute('data-og-state');
    if (!initialState) throw new Error('No initial state found');
    this.initialState = JSON.parse(initialState) as CurrentlyPlayingView;

    this.roomMessageListener = roomMessageListener;

    this.auxAudioPlayer = auxAudioPlayer;

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

}
