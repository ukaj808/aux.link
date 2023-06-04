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

type RoomMessageType = "ServerWelcomeMessage" | "UserEnterEvent" | "UserLeftEvent";

type ServerWelcomeMessage = {
    userId: string,
    userName: string,
    isCreator: boolean,
}

type UserEnterEvent = {
    userId: string,
    userName: string,
}

type UserLeftEvent = {
    userId: string
}

type RoomMessage = (ServerWelcomeMessage | UserEnterEvent | UserLeftEvent) & { type: RoomMessageType };

type AudioChunk = ArrayBuffer;

type EnqueueSongRequest = {
    priority: number,
}