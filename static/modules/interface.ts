type WsWorkerOpts = {
    type: "INIT", 
    roomId: string, 
    userId: string, 
    ringBuffer: SharedArrayBuffer,
    state: SharedArrayBuffer,
    readerOffset: SharedArrayBuffer,
    writerOffset: SharedArrayBuffer
    samplesRead: SharedArrayBuffer,
    samplesWritten: SharedArrayBuffer,
}

type AudioStreamPrepOptions = {
    ringBufferSize: number,
}

type WsSongFinishedEvent = {
    type: "SONG_FINISHED";
    offset: number;
}

type WsWorkerMessage = {
    type: "WS_WORKER_READY" | "SONG_STARTED"
} | WsSongFinishedEvent


type AudioWorkletMessage = {
    type: "SONG_STARTED" | "SONG_FINISHED",
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