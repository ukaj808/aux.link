// Global

import { AuxAudioPlayer } from "./aux-audio-player.js";
import { RoomMessageListener } from "./room-message-listener.js";
import { OrderElement } from "./order-element.js";
import { CurrentlyPlayingElement } from "./currently-playing-element.js";

let roomId    = location.pathname.substr(1);
let listening = false;
let userId;
let userName;

const auxAudioPlayer          = new AuxAudioPlayer({roomId: roomId});

const orderElement            = new OrderElement();
const currentlyPlayingElement = new CurrentlyPlayingElement({auxAudioPlayer});

const roomMessageListener     = new RoomMessageListener(
    roomId,
    orderElement
);
// Connect to web socket AFTER all the room modules have loaded (e.g. Order Section)
// Each module adds event listeners to the room page (main)...
// If the websocket connection happens before the event listeners are added in each module
// then those modules won't know that they/or another user joined...
// Still noticing this bug in prod!!
window.addEventListener('load', roomMessageListener.start())
