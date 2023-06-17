type WsWorkerOpts = {
    type: "INIT", 
    roomId: string, 
    userId: string, 
    ringBuffer: SharedArrayBuffer,
    state: SharedArrayBuffer,
}

type AudioStreamPrepOptions = {
    audioCtxOpts: AudioContextOptions,
    ringBufferSize: number
}

type WsWorkerMessage = {
    type: "WS_WORKER_READY";
}

type RoomMessageType = "ServerWelcomeCommand" | "ServerPrepareAudioCommand" | "ServerUploadSongCommand" | "SongStartingEvent" | "SongFinishedEvent" | "UserEnterEvent" | "UserLeftEvent";
type UserMessageType = "UserAudioPreparedEvent"

type SongStartingEvent = {
    s: number,
}

type ServerWelcomeCommand = {
    userId: string,
    userName: string,
    isCreator: boolean,
}

type ServerPrepareAudioCommand = {
// todo
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

type RoomMessage = (ServerWelcomeCommand | ServerUploadSongCommand | UserEnterEvent | UserLeftEvent | SongStartingEvent) & { type: RoomMessageType };
type UserMessage = (UserAudioPreparedEvent) & { type: UserMessageType };

type AudioChunk = ArrayBuffer;

type EnqueueSongRequest = {
    priority: number,
}

type AudioEvent = SongStartingEvent;