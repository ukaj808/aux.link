
export function fromPollingToCountdown(loadingBars: HTMLDivElement, countdownTimer: HTMLSpanElement) {
    loadingBars.classList.add('hidden');
    countdownTimer.classList.remove('hidden');
}