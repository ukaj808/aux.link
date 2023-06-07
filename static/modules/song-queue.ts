export class SongQueue {
    private queue: File[];

    constructor() {
        this.queue = [];
    }

    public addSongToQueue(file: File) {
        this.queue.push(file);
    }

    public dequeueSong(): File | undefined {
        return this.queue.shift();
    }

    public reindexSong(index: number, newIndex: number) {
        const song = this.queue[index];
        this.queue.splice(index, 1);
        this.queue.splice(newIndex, 0, song);
    }

    public swapSongs(index1: number, index2: number) {
        const temp = this.queue[index1];
        this.queue[index1] = this.queue[index2];
        this.queue[index2] = temp;
    }

    public removeSong(index: number) {
        this.queue.splice(index, 1);
    }

    public get length(): number {
        return this.queue.length;
    }
}