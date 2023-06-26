type FmtSubChunk = {
    subchunk1Id: string,
    subchunk1Size: number,
    audioFormat: number,
    numChannels: number,
    sampleRate: number,
    byteRate: number,
    blockAlign: number,
    bitsPerSample: number,
    cbSize: number,
    validBitsPerSample: number,
    channelMask: number,
}

type WsWorkerOpts = {
    type: "INIT", 
    roomId: string, 
    userId: string, 

    writeSharedBuffers: WriteSharedBuffers,
    buffers: WsBuffers,
    _audioWorkletOwnedBuffers: AudioWorkletBuffers,
}

// Only the ws worker has access to write to these buffers
type WsBuffers = {
    writerOffset: SharedArrayBuffer,
    samplesWritten: SharedArrayBuffer,
}

type WsBuffersView = {
    writerOffset: Int32Array,
    samplesWritten: Int32Array,
}

// Only the audio worklet has access to write from these buffers
type AudioWorkletBuffers = {
    readerOffset: SharedArrayBuffer,
    samplesRead: SharedArrayBuffer,
}

type AudioWorkletBuffersView = {
    readerOffset: Int32Array,
    samplesRead: Int32Array,
}

// Both the audio worklet and the ws worker have access to write to these buffers
type WriteSharedBuffers = {
    ringBuffer: SharedArrayBuffer,
    bufferReady: SharedArrayBuffer,
    sampleIndexBreak: SharedArrayBuffer,
}

type WriteSharedBuffersView = {
    ringBuffer: Float32Array,
    ringBufferDataView: DataView,
    bufferReady: Uint8Array,
    sampleIndexBreak: Int32Array,
}

type AudioStreamPrepOptions = {
    ringBufferSize: number,
}

type WsSongFinishedEvent = {
    type: "WRITE_SONG_FINISHED";
    offset: number;
}

type WsWorkerEvent = {
    type: "WS_WORKER_READY" | "WRITE_SONG_STARTED"
} | WsSongFinishedEvent


type AudioWorkletCommand = {
    type: "WRITE_SONG_STARTED",
} | WsSongFinishedEvent;

type AudioWorkletEvent = {
    type: "READ_SONG_FINISHED",
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

type UrlExtract = { 
    url: string,
    title: string,
};