export class AudioVisualizer {
    private analyser: AnalyserNode;
    private canvas: HTMLCanvasElement;
    private canvasCtx: CanvasRenderingContext2D;
    private buffer: Float32Array;
    private drawVisual?: number;
  
    constructor(analyser: AnalyserNode, canvas: HTMLCanvasElement) {
      this.analyser = analyser;
      this.canvas = canvas;
      this.analyser.fftSize = 256;
      this.buffer = new Float32Array(this.analyser.frequencyBinCount);
      this.canvasCtx = this.canvas.getContext('2d')!;
      this.canvasCtx.clearRect(0, 0, this.canvas.width, this.canvas.height);
    }
  
    public start() {
      this.drawVisual = requestAnimationFrame(this.draw.bind(this));
    }
  
    public stop() {
      if (this.drawVisual) {
        cancelAnimationFrame(this.drawVisual);
      }
    }
  
    private draw() {
      this.drawVisual = requestAnimationFrame(this.draw.bind(this));
      this.analyser.getFloatFrequencyData(this.buffer);
      this.canvasCtx.fillStyle = 'rgb(48, 49, 53)';
      this.canvasCtx.fillRect(0, 0, this.canvasCtx.canvas.width, this.canvasCtx.canvas.height);
  
      const barWidth = (this.canvasCtx.canvas.width / this.analyser.frequencyBinCount) * 2.5;
      let barHeight;
  
      let x = 0;
  
      for (let i = 0; i < this.analyser.frequencyBinCount; i++) {
        barHeight = this.buffer[i];
  
        this.canvasCtx.fillStyle = `rgb(${barHeight + 100}, 50, 50)`;
        this.canvasCtx.fillRect(x, this.canvasCtx.canvas.height - barHeight / 2, barWidth, barHeight);
  
        x += barWidth + 1;
      }
    }
  }