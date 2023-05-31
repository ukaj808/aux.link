import { AuxAudioPlayer } from "../modules/aux-audio-player";
import { RoomMessageListener } from "../modules/room-message-listener";
import { OrderElement } from "../modules/order-element";
import { DropElement } from "../modules/drop-element"; 
import { CurrentlyPlayingElement } from "../modules/currently-playing-element";

let roomId: string    = location.pathname.substr(1);
let listening: boolean = false;
let userId: string;
let userName: string;

const auxAudioPlayer: AuxAudioPlayer = new AuxAudioPlayer(roomId);

const orderElement: OrderElement = new OrderElement();
const currentlyPlayingElement: CurrentlyPlayingElement 
    = new CurrentlyPlayingElement(auxAudioPlayer);
const dropElement: DropElement = new DropElement();
const roomMessageListener: RoomMessageListener = new RoomMessageListener(
    roomId,
    orderElement
);

// Connect to web socket AFTER all the room modules have loaded (e.g. Order Section)
// Each module adds event listeners to the room page (main)...
// If the websocket connection happens before the event listeners are added in each module
// then those modules won't know that they/or another user joined...
// Still noticing this bug in prod!!
window.addEventListener('load', () => roomMessageListener.start())
