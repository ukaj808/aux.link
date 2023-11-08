import { MutableStyleSheet } from "./stylesheet-manipulations";

type ResponsiveQueueAnimationStyleOptions = {
    orientation: "horizontal" | "vertical";
    spaceBetweenElements: number;
    elementWidth: number; // or element height?
    stylesheet: MutableStyleSheet;
    media: MediaQueryList
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

            const mediaMatch = opts.styleOptions.find((styleOpts) => styleOpts.media.matches);
            if (!mediaMatch) throw new Error("None of the stylesheets match the current media query");

            const keyframe = mediaMatch.orientation === "horizontal" ?
                { left: "1000px", offset: 0 } : { bottom: "1000px", offset: 0 };

            const enterAnimation = el.animate(
                [
                    keyframe
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
        leave: (id: string) => { 
            const { el, index } = (Array.from(opts.queue.children) as HTMLElement[]).reduce((acc, el, index) => {
                if (el.id === id) {
                    return { el, index };
                }
                return acc;
            }, { el: (undefined as HTMLElement | undefined), index: -1 });
            if (!el) throw new Error(`No element found with id ${id}`);

            // todo
            const mediaMatch = opts.styleOptions.find((styleOpts) => styleOpts.media.matches);
            if (!mediaMatch) throw new Error("None of the stylesheets match the current media query");
            const keyframe = mediaMatch.orientation === "horizontal" ?
                { left: "1000px", offset: 0 } : { bottom: "1000px", offset: 0 };

            const leaveAnimation = el.animate(
                [
                    keyframe
                ],
                {
                  duration: 1000,
                }
              );
              const i = runningAnimations.push(leaveAnimation);
              leaveAnimation.finished.then(() => {
                opts.styleOptions.forEach((styleOpts) => styleOpts.stylesheet.delete(id));
                el.remove();
                runningAnimations.splice(i, 1);
              });

              // todo

            
        },
        cycle: () => { },
        cancel: () => { },
    }
}