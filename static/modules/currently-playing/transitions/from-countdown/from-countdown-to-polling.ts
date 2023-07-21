
export function fromCountdownToPolling(countdownTimer: HTMLSpanElement, loadingBars: HTMLDivElement) {
  countdownTimer.classList.add('hidden');
  loadingBars.classList.remove('hidden');
}