import { AuxAudioPlayer } from "./aux-audio-player";

export class CurrentlyPlayingElement {

  private el: HTMLElement;
  private auxAudioPlayer: AuxAudioPlayer;
  private analyser: AnalyserNode;
  private listening: boolean;
  private audioCanvas: HTMLCanvasElement;
  private canvasCtx: CanvasRenderingContext2D;
  private buffer: Uint8Array;
  private drawVisual?: number;

  constructor(auxAudioPlayer: AuxAudioPlayer, analyser: AnalyserNode) {
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

    this.listening = false;
    this.auxAudioPlayer = auxAudioPlayer;
    this.analyser = analyser;
    this.analyser.fftSize = 256;
    this.buffer = new Uint8Array(this.analyser.frequencyBinCount);

    this.el.addEventListener("click", () => this.onSectionClick());
  }

  private onSectionClick() {
    if (!this.listening) {
        this.auxAudioPlayer.startListening();
        this.listening = true;
        this.toggleDisconnectOverlay();

        const draw = () => {
          this.drawVisual = requestAnimationFrame(draw);
          this.analyser.getByteFrequencyData(this.buffer);
          this.canvasCtx.fillStyle = 'rgb(0, 0, 0)';
          this.canvasCtx.fillRect(0, 0, this.audioCanvas.width, this.audioCanvas.height);


          const barWidth = (this.audioCanvas.width / this.analyser.frequencyBinCount) * 2.5;
          let barHeight;

          let x = 0;

          for (let i = 0; i < this.analyser.frequencyBinCount; i++) {
            barHeight = this.buffer[i] / 2;
        
            this.canvasCtx.fillStyle = `rgb(${barHeight + 100}, 50, 50)`;
            this.canvasCtx.fillRect(x, this.audioCanvas.height - barHeight / 2, barWidth, barHeight);
        
            x += barWidth + 1;
          }
        }
        draw();
    } else {
        this.auxAudioPlayer.stopListening();
        this.listening = false;
    }
  }

  private toggleDisconnectOverlay() {
    this.el.classList.toggle("overlay");
  }

}
