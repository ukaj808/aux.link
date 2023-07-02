import Sortable from "sortablejs";
import { RestClient } from "./rest-client";
import { SongQueue } from "./song-queue";
import { AuxAudioPlayer } from "./aux-audio-player";
import { HtmlParser } from "./cheerio-service";

export class DropElement {
    private el: HTMLDivElement;
    private dropZoneEl: HTMLLabelElement;
    private dropZoneInputEl: HTMLInputElement;
    private dropZonePasteHackInputEl: HTMLInputElement;
    private sortableList: Sortable | undefined;
    private queue: SongQueue;
    private restClient: RestClient;
    private auxAudioPlayer: AuxAudioPlayer;
    private htmlParser: HtmlParser;

    //private onContextMenuBound: (e: MouseEvent) => void;

    constructor(restClient: RestClient, auxAudioPlayer: AuxAudioPlayer, htmlParser: HtmlParser) {
        const el = document.getElementById("drop");
        if (!el) throw new Error('No drop element found');
        this.el = el as HTMLDivElement;

        const dropZoneEl = document.getElementById("drop-zone");
        if (!dropZoneEl) throw new Error('No drop element found');
        this.dropZoneEl = dropZoneEl as HTMLLabelElement;

        const dropZoneInputEl = document.getElementById("drop-zone-input");
        if (!dropZoneInputEl) throw new Error('No drop element input found');
        this.dropZoneInputEl = dropZoneInputEl as HTMLInputElement;

        const dropZonePasteHackInputEl = document.getElementById("drop-zone-paste-hack");
        if (!dropZonePasteHackInputEl) throw new Error('No drop element input hack found');
        this.dropZonePasteHackInputEl = dropZonePasteHackInputEl as HTMLInputElement;

        this.dropZoneEl.addEventListener('drop', this.onDrop.bind(this));
        this.dropZoneEl.addEventListener('dragover', this.onDragOver.bind(this));
        this.dropZoneEl.addEventListener('paste', this.onPaste.bind(this));
        this.dropZoneInputEl.addEventListener('input', this.onInputChange.bind(this));
        this.dropZoneInputEl.addEventListener('click', this.onInputClick.bind(this));
        this.dropZonePasteHackInputEl.addEventListener('focus', this.onHackInputFocus.bind(this));
        this.queue = new SongQueue();
        this.restClient = restClient;
        this.auxAudioPlayer = auxAudioPlayer;
        this.htmlParser = htmlParser
    }
    
    public async uploadAndDequeueSong() {
        const song = this.queue.peekSong();
        if (song == null) throw new Error('No song found');
        await this.restClient.uploadSong(song);
        return this.dequeueSong();
    }

    private onHackInputFocus(e: FocusEvent) {
        e.preventDefault();
        this.dropZoneEl.focus();
    }


    private async onPaste(e: ClipboardEvent) {
        e.preventDefault();
        if (e.clipboardData == null) throw new Error('No clipboard data found');
        // check if its a file
        if (e.clipboardData.files.length > 0) {
            [...e.clipboardData.files].forEach((file) => this.addSongToQueue(file));
            return;
        }
        // check if its a url
        let pastedText = e.clipboardData.getData("text");
        if (!this.isValidHttpUrl(pastedText)) return;
        try {
            const title = await this.restClient.validateUrl(pastedText);
            if (!title) {
                console.warn("Invalid url!")
                return;
            }
            this.addSongToQueue({title, url: pastedText});
        } catch (e) {
            // TODO: show error to user
            console.error(e);
        }
    }

    private async extractTitleFromUrl(url: string) {
        const html = await this.restClient.getHtml(url);
        return this.htmlParser.getTitle(html);
    }

    private isValidHttpUrl(text: string) {
        let url;
        try {
            url = new URL(text);
        } catch (_) {
            return false;  
        }
        return url.protocol === "http:" || url.protocol === "https:";
    }

    private onDrop(e: DragEvent) {
        e.preventDefault();
        // @ts-ignore
        if (e.dataTransfer == null) throw new Error('No data transfer found');
        // Use DataTransferItemList interface to access the file(s)
        [...e.dataTransfer.items].forEach((item, i) => {
            // If dropped items aren't files, reject them
            if (item.kind === "file") {
                const file = item.getAsFile();
                if (file == null) throw new Error('No file found');
                this.addSongToQueue(file);
            }
        });
    }

    private onDragOver(e: Event) {
        // Prevent default behavior (Prevent file from being opened)
        e.preventDefault();
    }

    private onInputChange(e: Event) {
        const inputTarget = e.target as HTMLInputElement;
        const files = inputTarget.files;
        if (files == null) throw new Error('No files found');
        [...files].forEach((file) => this.addSongToQueue(file));
    }

    /*
    * This is a hack to allow the user to upload the same file twice
    * https://stackoverflow.com/questions/12030686/html-input-file-selection-event-not-firing-upon-selecting-the-same-file
    */
    private onInputClick(e: Event) {
        const inputTarget = e.target as HTMLInputElement;
        inputTarget.value = ''; // clear input value
    }

    private addSongToQueue(file: Song) {
        if (this.queue.length === 0) {
            this.initSortableList(file);
        } else {
            this.addSongToSortableList(file);
        }
        this.queue.addSongToQueue(file);
    }

    private dequeueSong() {
        if (this.sortableList == null) throw new Error('No sortable list found');
        if (this.sortableList.el.firstChild == null) throw new Error('No sortable list first child found');
        this.sortableList.el.removeChild(this.sortableList.el.firstChild);
        return this.queue.dequeueSong();
    }

    private shiftToListContain() {
        this.dropZoneEl.classList.add('list-contain');
    }

    private initSortableList(song: Song) {
        this.shiftToListContain();
        const songQueueEl = document.createElement('ol');
        songQueueEl.classList.add('song-queue-list');
        this.sortableList = new Sortable(songQueueEl, {});
        this.clearDropZoneChildrenEls();
        this.dropZoneEl.appendChild(songQueueEl);
        this.addSongToSortableList(song);
    }

    private clearDropZoneChildrenEls() {
        Array.from(this.dropZoneEl.children).forEach((child) => {
            if (child === this.dropZoneInputEl) return;
            this.dropZoneEl.removeChild(child);
        });
    }

    private addSongToSortableList(song: Song) {
        if (this.sortableList == null) throw new Error('No sortable list found');
        const songEl = document.createElement('li');
        songEl.classList.add('song-list-item');
        // if its a file
        if (song instanceof File) {
            songEl.innerText = (song as File).name;
        } else {
            songEl.innerText = (song as UrlUpload).title;
        };
        this.sortableList.el.appendChild(songEl);
    }

}
