type WsWorkerOpts = {
    type: "INIT", 
    roomId: string, 
    userId: string, 
    ringBuffer: SharedArrayBuffer,
    state: SharedArrayBuffer,
    audioWorkletOffset: SharedArrayBuffer,
    audioWorkletLap: SharedArrayBuffer,
    samplesRead: SharedArrayBuffer,
    samplesWritten: SharedArrayBuffer,
    wsWorkerOffset: SharedArrayBuffer
    wsWorkerLap: SharedArrayBuffer
}

type AudioStreamPrepOptions = {
    ringBufferSize: number,
}

type WsWorkerMessage = {
    type: "WS_WORKER_READY" | "SONG_STARTED" | "SONG_FINISHED";
}

type AudioWorkletMessage = {
    type: "SONG_STARTED" | "SONG_FINISHED";
}

type RoomMessageType = "ServerWelcomeCommand" | "ServerUploadSongCommand" | "SongStartingEvent" | "SongUploadedEvent" | "UserEnterEvent" | "UserLeftEvent";

type SongStartingEvent = {
    s: number,
}

type ServerWelcomeCommand = {
    userId: string,
    userName: string,
    isCreator: boolean,
}

type SongUploadedEvent = {

}

type UserEnterEvent = {
    userId: string,
    userName: string,
}

type ServerUploadSongCommand = {}
type SongFinishedEvent = {}
type UserAudioPreparedEvent = {
    userId: string
}
type UserLeftEvent = {
    userId: string
}

type RoomMessage = (ServerWelcomeCommand | ServerUploadSongCommand | UserEnterEvent | UserLeftEvent | SongUploadedEvent | SongStartingEvent) & { type: RoomMessageType };

type AudioChunk = ArrayBuffer;

type EnqueueSongRequest = {
    priority: number,
}

type AudioEvent = SongStartingEvent;