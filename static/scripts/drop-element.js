export class DropElement {

    #el;
    #queue;

    constructor() {
        this.#el = document.getElementById("drop-zone");
        this.#el.addEventListener("drop", this.#onDrop);
        this.#el.addEventListener("dragover", this.#onDragOver);
        this.#queue = [];
    }

    #onDrop(e) {
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

    #onDragOver(e) {
        // Prevent default behavior (Prevent file from being opened)
        e.preventDefault();
    }

}
