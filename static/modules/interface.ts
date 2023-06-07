type WsWorkerOpts = {
    type: "INIT", 
    roomId: string, 
    userId: string, 
    ringBuffer: SharedArrayBuffer,
    state: SharedArrayBuffer,
}

type WsWorkerMessage = {
    type: "WS_WORKER_READY";
} | SongStartingEvent;

type SongStartingEvent = {
    type: "SONG_STARTING",
    timeLeftInSeconds: number,
};

type RoomMessageType = "ServerWelcomeMessage" | "UploadSongMessage" | "UserEnterEvent" | "UserLeftEvent";

type ServerWelcomeMessage = {
    userId: string,
    userName: string,
    isCreator: boolean,
}

type UserEnterEvent = {
    userId: string,
    userName: string,
}

type UploadSongMessage = {}

type UserLeftEvent = {
    userId: string
}

type RoomMessage = (ServerWelcomeMessage | UserEnterEvent | UserLeftEvent) & { type: RoomMessageType };

type AudioChunk = ArrayBuffer;

type EnqueueSongRequest = {
    priority: number,
}

type AudioEvent = SongStartingEvent;