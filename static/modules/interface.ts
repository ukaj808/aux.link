type WsWorkerOpts = {
    type: "INIT", 
    roomId: string, 
    userId: string, 
    ringBuffer: SharedArrayBuffer,
    state: SharedArrayBuffer,
}

type WsWorkerMessage = {
    type: "WS_WORKER_READY";
}

type RoomMessageType = "ServerWelcomeMessage" | "ServerUploadSongMessage" | "SongStartingEvent" | "UserEnterEvent" | "UserLeftEvent";

type SongStartingEvent = {
    s: number,
}

type ServerWelcomeMessage = {
    userId: string,
    userName: string,
    isCreator: boolean,
}

type UserEnterEvent = {
    userId: string,
    userName: string,
}

type ServerUploadSongMessage = {}

type UserLeftEvent = {
    userId: string
}

type RoomMessage = (ServerWelcomeMessage | ServerUploadSongMessage | UserEnterEvent | UserLeftEvent | SongStartingEvent) & { type: RoomMessageType };

type AudioChunk = ArrayBuffer;

type EnqueueSongRequest = {
    priority: number,
}

type AudioEvent = SongStartingEvent;