fetch("/public/web-components/queue-section/queue-section.html")
    .then(stream => stream.text())
    .then(text => define(text));

export const define = (html) => {
    class QueueSection extends HTMLElement {
        constructor() {
            super();
            const shadow = this.attachShadow({mode: 'open'});
            shadow.innerHTML = html;
        }
    }
    customElements.define('queue-section', QueueSection);
}
