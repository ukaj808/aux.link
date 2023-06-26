export class SongQueue {
    private queue: (File | UrlExtract)[];

    constructor() {
        this.queue = [];
    }

    public addSongToQueue(file: File | UrlExtract) {
        this.queue.push(file);
    }

    public dequeueSong(): (File | UrlExtract) | undefined {
        return this.queue.shift();
    }

    public peekSong(): (File | UrlExtract) | undefined {
        return this.queue[0];
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