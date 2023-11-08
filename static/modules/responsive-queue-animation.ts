import { MutableStyleSheet } from "./stylesheet-manipulations";

type ResponsiveQueueAnimationStyleOptions = {
    orientation: string;
    spaceBetweenElements: number;
    elementWidth: number; // or element height?
    stylesheet: MutableStyleSheet;
};

type ResponsiveQueueAnimationOptions = {
    queue: HTMLElement;
    childElementType: keyof HTMLElementTagNameMap;
    childElementClassName: string;
    styleOptions: ResponsiveQueueAnimationStyleOptions[];
}

type ResponsiveQueueAnimationManager = (opts: ResponsiveQueueAnimationOptions) => {
    enter: (id: string) => void;
    leave: (id: string) => void;
    cycle: () => void;
    cancel: () => void;
};

const responsiveQueueAnimationManager: ResponsiveQueueAnimationManager = (opts) => {
    const runningAnimations: Animation[] = [];
    const endPositions = opts.styleOptions.map(() => 0); 

    // 1. Initially set the position of each element in the queue
    //    based on the orientation of the queue and the space between.
    //    They should be positioned absolutely and fixed to one side of the queue.
    //
    //    e.g. horizontal = top,left,bottom: 0, vertical = top,left,right: 0
    Array.from(opts.queue.children).forEach((c, i) => {
        const el = c as HTMLElement;
        opts.styleOptions.forEach((styleOpts, j) => {
            let declarations = new Map<string, string>(styleOpts.stylesheet.get(el.id)?.declarations);
            const stacking = styleOpts.spaceBetweenElements < 0;
            if (stacking) {
                const dataZIndex = c.getAttribute("data-z-index");
                if (!dataZIndex) throw new Error(`A negative spaceBetweenElements (${styleOpts.spaceBetweenElements}) requires a data-z-index attribute on the element`);
                declarations.set("z-index", dataZIndex);
            }
            switch (styleOpts.orientation) {
                case "horizontal":
                    const dataLeft = el.getAttribute("data-left");
                    if (!dataLeft) throw new Error("A horizontal orientation requires a data-left attribute on the element");
                    declarations.set("left", dataLeft);
                    break;
                case "vertical":
                    const dataTop = el.getAttribute("data-top");
                    if (!dataTop) throw new Error("A vertical orientation requires a data-top attribute on the element");
                    declarations.set("top", dataTop);
                    break;
            }
            styleOpts.stylesheet.put(el.id, declarations);
        });
    });

    return {
        enter: (id: string) => {
            // 1. if stacking, rezindex previous elements 
            opts.styleOptions.forEach((styleOpts) => {
                const stacking = styleOpts.spaceBetweenElements < 0;
                if (stacking) {
                    for (
                        let i = 0, j = opts.queue.childElementCount;
                        i < opts.queue.childElementCount;
                        i++, j--
                    ) {
                        const el = opts.queue.children[i] as HTMLElement;
                        const declarations = new Map(styleOpts.stylesheet.get(el.id)?.declarations);
                        declarations.set("z-index", j.toString());
                        styleOpts.stylesheet.put(el.id, declarations);
                    }
                }
            });

            const el = document.createElement(opts.childElementType);
            el.id = id;
            el.classList.add(opts.childElementClassName); // need to pass this class in somehow; it must align with the orientation restraints!!

            opts.styleOptions.forEach((styleOpts, i) => {
                const declarations = new Map<string, string>();
                const stacking = styleOpts.spaceBetweenElements < 0;
                if (stacking) {
                    declarations.set("z-index", "0");
                }
                const endPosition = endPositions[i] + styleOpts.spaceBetweenElements + styleOpts.elementWidth;
                switch (styleOpts.orientation) {
                    case "horizontal":
                        declarations.set("left", endPosition + "px");
                        break;
                    case "vertical":
                        declarations.set("top", endPosition + "px");
                        break;
                
                }
                styleOpts.stylesheet.put(el.id, declarations);
                endPositions[i] = endPosition;
            });

            opts.queue.appendChild(el);

            const enterAnimation = el.animate(
                [
                    {
                        left: "1000px", // should I pass this in?
                        offset: 0,
                    },
                ],
                {
                    duration: 10000,
                }
            );
            const index = runningAnimations.push(enterAnimation);
            enterAnimation.finished.then(() => {
                runningAnimations.splice(index, 1);
            });

        },
        leave: (id: string) => { },
        cycle: () => { },
        cancel: () => { },
    }
}