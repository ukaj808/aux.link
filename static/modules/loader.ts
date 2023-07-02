export class LoaderFactory {

    constructor() {
    }

    public generateLoadingBars(): HTMLDivElement {
        var outerDiv = document.createElement("div") as HTMLDivElement;
        outerDiv.className = "lds-facebook";

        var innerDiv1 = document.createElement("div");
        var innerDiv2 = document.createElement("div");
        var innerDiv3 = document.createElement("div");

        outerDiv.appendChild(innerDiv1);
        outerDiv.appendChild(innerDiv2);
        outerDiv.appendChild(innerDiv3);

        return outerDiv;
    }


}
