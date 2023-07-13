import { AuxAudioPlayer } from "../modules/aux-audio-player";
import { RoomMessageListener } from "../modules/room-message-listener";
import { OrderElement } from "../modules/order-element";
import { DropElement } from "../modules/drop-element"; 
import { CurrentlyPlayingElement } from "../modules/currently-playing/currently-playing-element";
import { RestClient } from "../modules/rest-client";
import { SvgFactory } from "../modules/svg";
import { SongQueue } from "../modules/song-queue";
import { LoaderFactory } from "../modules/loader";

let roomId: string    = location.pathname.substr(1);
let listening: boolean = false;

const roomMessageListener: RoomMessageListener = new RoomMessageListener(roomId);
const audioContext = new AudioContext({
    latencyHint: "playback",
    sampleRate: 48000,
});
const analyser = audioContext.createAnalyser();
const svgFactory: SvgFactory = new SvgFactory();
const loaderFactory: LoaderFactory = new LoaderFactory();
const auxAudioPlayer: AuxAudioPlayer = new AuxAudioPlayer(roomId, audioContext, analyser, roomMessageListener);
const restClient: RestClient = new RestClient(roomId, roomMessageListener);
const orderElement: OrderElement = new OrderElement(roomMessageListener, restClient, svgFactory);
const currentlyPlayingElement: CurrentlyPlayingElement 
    = new CurrentlyPlayingElement(roomMessageListener, auxAudioPlayer, analyser);
const dropElement: DropElement = new DropElement(roomMessageListener, restClient, loaderFactory, svgFactory);
roomMessageListener.start();