import Sortable from "sortablejs";

export class DropElement {
    el: HTMLDivElement;
    dropZoneEl: HTMLLabelElement;
    dropZoneInputEl: HTMLInputElement;
    sortableList: Sortable | undefined;
    queue: File[];

    constructor() {
        const el = document.getElementById("drop");
        if (!el) throw new Error('No drop element found');
        this.el = el as HTMLDivElement;

        const dropZoneEl = document.getElementById("drop-zone");
        if (!dropZoneEl) throw new Error('No drop element found');
        this.dropZoneEl = dropZoneEl as HTMLLabelElement;

        const dropZoneInputEl = document.getElementById("drop-zone-input");
        if (!dropZoneInputEl) throw new Error('No drop element input found');
        this.dropZoneInputEl = dropZoneInputEl as HTMLInputElement;

        this.dropZoneEl.addEventListener('drop', this.onDrop.bind(this));
        this.dropZoneEl.addEventListener('dragover', this.onDragOver.bind(this));
        this.dropZoneInputEl.addEventListener('change', this.onInputChange.bind(this));
        this.queue = [];
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
                console.log(`… file[${i}].name = ${file.name}`);
                this.addSongToQueue(file);
            }
        });
    }

    private onDragOver(e: Event) {
        // Prevent default behavior (Prevent file from being opened)
        console.log('drag over');
        e.preventDefault();
    }

    private onInputChange(e: Event) {
        const inputTarget = e.target as HTMLInputElement;
        const files = inputTarget.files;
        if (files == null) throw new Error('No files found');
        [...files].forEach((file) => this.addSongToQueue(file));
    }

    private addSongToQueue(file: File) {
        if (this.queue.length === 0) {
            this.initSortableList(file);
        } else {
            this.addSongToSortableList(file);
        }
        this.queue.push(file);
    }

    private shiftToListContain() {
        this.dropZoneEl.classList.add('list-contain');
    }

    private initSortableList(file: File) {
        this.shiftToListContain();
        const songQueueEl = document.createElement('ol');
        songQueueEl.classList.add('song-queue-list');
        this.sortableList = new Sortable(songQueueEl, {});
        this.clearDropZoneChildrenEls();
        this.dropZoneEl.appendChild(songQueueEl);
        this.addSongToSortableList(file);
    }

    private clearDropZoneChildrenEls() {
        Array.from(this.dropZoneEl.children).forEach((child) => {
            if (child === this.dropZoneInputEl) return;
            this.dropZoneEl.removeChild(child);
        });
    }

    private addSongToSortableList(file: File) {
        if (this.sortableList == null) throw new Error('No sortable list found');
        const songEl = document.createElement('li');
        songEl.classList.add('song-list-item');
        songEl.innerText = file.name;
        this.sortableList.el.appendChild(songEl);
    }

}
