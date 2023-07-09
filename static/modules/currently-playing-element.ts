import { AuxAudioPlayer } from "./aux-audio-player";
import { RoomMessageListener } from "./room-message-listener";

export class CurrentlyPlayingElement {

  private el: HTMLElement;
  private roomMessageListener: RoomMessageListener;
  private auxAudioPlayer: AuxAudioPlayer;
  private analyser: AnalyserNode;
  private listening: boolean;
  private audioCanvas: HTMLCanvasElement;
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

    this.roomMessageListener = roomMessageListener;
    this.roomMessageListener.subscribe('SongStartingEvent', (data) => {
      const songStartingEvent = data as SongStartingEvent;
    });
    this.listening = false;
    this.auxAudioPlayer = auxAudioPlayer;
    this.analyser = analyser;
    this.analyser.fftSize = 256;
    this.buffer = new Float32Array(this.analyser.frequencyBinCount);

    this.el.addEventListener("click", () => this.onSectionClick());
  }

  private showSecondsLeft(secondsLeft: number) {
    const secondSpan = document.createElement("span");
    secondSpan.classList.add("seconds-left");
    secondSpan.innerText = secondsLeft.toString();

    // store current child elements and structure

  }

  private onSectionClick() {
    if (!this.listening) {
        this.auxAudioPlayer.startListening();
        this.listening = true;
        this.toggleDisconnectOverlay();

        const draw = () => {
          this.drawVisual = requestAnimationFrame(draw);
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
        draw();
    } else {
        this.auxAudioPlayer.stopListening();
        this.listening = false;
        this.toggleDisconnectOverlay();
    }
  }

  private toggleDisconnectOverlay() {
    this.el.classList.toggle("overlay");
  }

}
