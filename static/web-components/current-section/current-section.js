fetch("/public/web-components/current-section/current-section.html")
    .then(stream => stream.text())
    .then(text => define(text));

const define = (html) => {
    const roomElement = document.getElementById("room");
    let timeSlider;
    let volSlider;
    let volLabel;
    let shadow;

    // Using "function" to bind "this" to input element.
    const bindVolSlider = function(){
        volLabel.innerHTML = this.value;
    }

    class CurrentSection extends HTMLElement {
        constructor() {
            super();
            shadow = this.attachShadow({mode: 'open'});
            shadow.innerHTML = html;
            timeSlider = shadow.getElementById("cur-time-input");
            volSlider = shadow.getElementById("cur-vol-input");
            volLabel = shadow.getElementById("cur-vol-lbl");
        }

        connectedCallback() {
            super.connectedCallback && super.connectedCallback();
            volSlider.addEventListener("input", bindVolSlider);
        }

        disconnectedCallback() {
            volSlider.removeEventListener("input", bindVolSlider);
            super.disconnectedCallback && super.disconnectedCallback();
        }
    }
    customElements.define('current-section', CurrentSection);
}
