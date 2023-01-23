fetch("/public/web-components/democracy-section/democracy-section.html")
    .then(stream => stream.text())
    .then(text => define(text));

export const define = (html) => {
    class DemocracySection extends HTMLElement {
        constructor() {
            super();
            const shadow = this.attachShadow({mode: 'open'});
            shadow.innerHTML = html;
        }
    }
    customElements.define('democracy-section', DemocracySection);
}
