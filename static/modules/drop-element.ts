export class DropElement {

    el: HTMLElement;
    queue: any[];

    constructor() {
        const optEl = document.getElementById("drop-zone");
        if (!optEl) throw new Error('No drop element found');
        this.el = optEl
        this.el.addEventListener("drop", this.onDrop);
        this.el.addEventListener("dragover", this.onDragOver);
        this.queue = [];
    }

    private onDrop(e: any) {
        if (e.dataTransfer.items) {
            // Use DataTransferItemList interface to access the file(s)
            [...e.dataTransfer.items].forEach((item, i) => {
                // If dropped items aren't files, reject them
                if (item.kind === "file") {
                    const file = item.getAsFile();

                }
            });
        } else {
            // Use DataTransfer interface to access the file(s)
            [...e.dataTransfer.files].forEach((file, i) => {
                console.log(`… file[${i}].name = ${file.name}`);
            });
        }
    }

    private onDragOver(e: Event) {
        // Prevent default behavior (Prevent file from being opened)
        e.preventDefault();
    }

}
