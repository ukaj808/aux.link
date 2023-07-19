export function fromConnectingToNotRunning(description: HTMLSpanElement) {
    description.classList.remove("hidden");
    description.innerText = "Waiting for the creator to start the music...";
}