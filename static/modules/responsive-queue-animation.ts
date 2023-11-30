import { MutableStyleSheet } from "./stylesheet-manipulations";

export type ResponsiveQueueAnimationStyleOptions = {
  orientation: "horizontal" | "vertical";
  spaceBetweenElements: number;
  stylesheet: MutableStyleSheet;
  media: MediaQueryList;
  startingTailPosition: number;
  elementWidth: number;
  elementHeight: number;
};

export type ResponsiveQueueAnimationOptions = {
  queue: HTMLElement;
  childElementType: keyof HTMLElementTagNameMap;
  childElementClassName: string;
  styleOptions: ResponsiveQueueAnimationStyleOptions[];
};

export type ResponsiveQueueAnimationManager = {
  enter: (id: string, styles?: Map<string, string>) => void;
  leave: (id: string) => void;
  cycle: () => void;
};

export const responsiveQueueAnimationManager = (
  opts: ResponsiveQueueAnimationOptions
): ResponsiveQueueAnimationManager => {
  const tailPositions = opts.styleOptions.map((s) => s.startingTailPosition);
  let animationCount = 0;

  // Listen for media query changes and finish all animations
  // so that animations dont bleed into other media query states
  opts.styleOptions.forEach((styleOpts) => {
    styleOpts.media.addEventListener("change", () => {
      opts.queue.getAnimations({ subtree: true }).forEach((a) => {
        a.finish();
      });
    });
  });

  const animate = (
    el: HTMLElement,
    keyframes: Keyframe[] | PropertyIndexedKeyframes | null,
    options?: KeyframeAnimationOptions
  ): Animation => {
    const animationId = (animationCount++).toString();
    const optsWithId = { ...options, id: animationId };
    return el.animate(keyframes, optsWithId);
  };

  // A function which iterates through the queue and "rezindexes" the elements
  // based on their position in the queue. This is necessary for stacking elements.
  // Should theoretically be called whenever an element is added or removed from the queue.
  const rezindex = (diff: 1 | 0 = 0) => {
    opts.styleOptions.forEach((styleOpts) => {
      const stacking = styleOpts.spaceBetweenElements < 0;
      if (stacking) {
        for (
          let i = 0, j = opts.queue.childElementCount - 1 + diff;
          i < opts.queue.childElementCount;
          i++, j--
        ) {
          const el = opts.queue.children[i] as HTMLElement;
          const declarations = new Map(
            styleOpts.stylesheet.get(el.id)?.declarations
          );
          declarations.set("z-index", j.toString());
          styleOpts.stylesheet.put(el.id, declarations);
        }
      }
    });
  };

  const getCurrentValue = (
    styleOptions: ResponsiveQueueAnimationStyleOptions,
    id: string
  ): number => {
    const el = document.getElementById(id);
    if (!el) throw new Error(`No element found with id ${id}`);
    const declarations = styleOptions.stylesheet.get(el.id)?.declarations;
    if (!declarations)
      throw new Error(`No declarations found for element with id ${id}`);
    const cssAttr = styleOptions.orientation === "horizontal" ? "left" : "top";
    const cssValue = declarations.get(cssAttr);
    if (!cssValue)
      throw new Error(`No css value found for element with id ${id}`);
    return parseInt(cssValue);
  };

  const updateCurrentValue = (
    styleOpts: ResponsiveQueueAnimationStyleOptions,
    id: string,
    value: number
  ) => {
    const el = document.getElementById(id);
    if (!el) throw new Error(`No element found with id ${id}`);
    const declarations = new Map(styleOpts.stylesheet.get(el.id)?.declarations);
    const cssAttr = styleOpts.orientation === "horizontal" ? "left" : "top";
    declarations.set(cssAttr, value.toString() + "px");
    styleOpts.stylesheet.put(el.id, declarations);
  };

  const moveElementsOneUpAnimation = (
    start: number,
    end?: number,
    finished?: () => void // ???
  ): void => {
    Array.from(opts.queue.children)
      .slice(start, end)
      .forEach((c) => {
        const el = c as HTMLElement;
        opts.styleOptions.forEach((styleOpts) => {
          const declarations = new Map(
            styleOpts.stylesheet.get(el.id)!.declarations
          );
          const attr = styleOpts.orientation === "horizontal" ? "left" : "top";
          const prevValueAsStr =
            styleOpts.orientation === "horizontal"
              ? declarations.get("left")
              : declarations.get("top");
          const prevValue = parseInt(prevValueAsStr!);
          const difference =
            styleOpts.orientation === "horizontal"
              ? styleOpts.elementWidth
              : styleOpts.elementHeight;
          const newValue =
            prevValue - (styleOpts.spaceBetweenElements + difference);
          declarations.set(attr, newValue + "px");
          styleOpts.stylesheet.put(el.id, declarations);

          if (styleOpts.media.matches) {
            const moveUpKeyframe = { [attr]: prevValueAsStr, offset: 0 };
            animate(el, [moveUpKeyframe], { duration: 1000 });
          }
        });
      });
  };

  // 1. Initially set the position of each element in the queue
  //    based on the orientation of the queue and the space between.
  //    They should be positioned absolutely and fixed to one side of the queue.
  //
  //    e.g. horizontal = top,left,bottom: 0, vertical = top,left,right: 0
  Array.from(opts.queue.children).forEach((c, i) => {
    const el = c as HTMLElement;
    opts.styleOptions.forEach((styleOpts, j) => {
      let declarations = new Map<string, string>(
        styleOpts.stylesheet.get(el.id)?.declarations
      );
      const stacking = styleOpts.spaceBetweenElements < 0;
      if (stacking) {
        const dataZIndex = c.getAttribute("data-z-index");
        if (!dataZIndex)
          throw new Error(
            `A negative spaceBetweenElements (${styleOpts.spaceBetweenElements}) requires a data-z-index attribute on the element`
          );
        declarations.set("z-index", dataZIndex);
      }
      switch (styleOpts.orientation) {
        case "horizontal":
          const dataLeft = el.getAttribute("data-left");
          if (!dataLeft)
            throw new Error(
              "A horizontal orientation requires a data-left attribute on the element"
            );
          declarations.set("left", dataLeft);
          break;
        case "vertical":
          const dataTop = el.getAttribute("data-top");
          if (!dataTop)
            throw new Error(
              "A vertical orientation requires a data-top attribute on the element"
            );
          declarations.set("top", dataTop);
          break;
      }
      tailPositions[j] =
        tailPositions[j] +
        styleOpts.spaceBetweenElements +
        (styleOpts.orientation === "horizontal"
          ? styleOpts.elementWidth
          : styleOpts.elementHeight);
      styleOpts.stylesheet.put(el.id, declarations);
    });
  });

  return {
    enter: (id: string, styles?: Map<string, string>) => {
      rezindex(1);

      const el = document.createElement(opts.childElementType);
      el.id = id;
      el.classList.add(opts.childElementClassName); // need to pass this class in somehow; it must align with the orientation restraints!!

      if (styles) {
        styles.forEach((value, key) => {
          el.style.setProperty(key, value);
        });
      }

      opts.styleOptions.forEach((styleOpts, i) => {
        const declarations = new Map<string, string>();
        const stacking = styleOpts.spaceBetweenElements < 0;
        if (stacking) {
          declarations.set("z-index", "0");
        }
        const elementDimension =
          styleOpts.orientation === "horizontal"
            ? styleOpts.elementWidth
            : styleOpts.elementHeight;
        switch (styleOpts.orientation) {
          case "horizontal":
            declarations.set("left", tailPositions[i] + "px");
            break;
          case "vertical":
            declarations.set("top", tailPositions[i] + "px");
            break;
        }
        styleOpts.stylesheet.put(el.id, declarations);
        tailPositions[i] =
          tailPositions[i] + styleOpts.spaceBetweenElements + elementDimension;
      });

      opts.queue.appendChild(el);

      const mediaMatch = opts.styleOptions.find(
        (styleOpts) => styleOpts.media.matches
      );
      if (!mediaMatch)
        throw new Error(
          "None of the stylesheets match the current media query"
        );

      const keyframe =
        mediaMatch.orientation === "horizontal"
          ? { left: "1000px", offset: 0 }
          : { top: "1000px", offset: 0 };

      animate(el, [keyframe], { duration: 1000 });
    },
    leave: (id: string) => {
      const { leavingEl, index } = (
        Array.from(opts.queue.children) as HTMLElement[]
      ).reduce(
        (acc, el, index) => {
          if (el.id === id) {
            return { leavingEl: el, index };
          }
          return acc;
        },
        { leavingEl: undefined as HTMLElement | undefined, index: -1 }
      );
      if (!leavingEl) throw new Error(`No element found with id ${id}`);

      // todo
      const mediaMatch = opts.styleOptions.find(
        (styleOpts) => styleOpts.media.matches
      );
      if (!mediaMatch)
        throw new Error(
          "None of the stylesheets match the current media query"
        );
      const leaveKeyframe =
        mediaMatch.orientation === "horizontal"
          ? { left: "1000px" }
          : { top: "1000px" };

      opts.styleOptions.forEach((styleOpts, i) => {
        tailPositions[i] =
          tailPositions[i] -
          (styleOpts.spaceBetweenElements +
            (styleOpts.orientation === "horizontal"
              ? styleOpts.elementWidth
              : styleOpts.elementHeight));
      });
      animate(leavingEl, [leaveKeyframe], { duration: 1000 }).finished.then(
        () => {
          opts.styleOptions.forEach((styleOpts) =>
            styleOpts.stylesheet.delete(id)
          );
          leavingEl.remove();
          rezindex();
        }
      );

      moveElementsOneUpAnimation(index+1, undefined);
    },
    cycle: () => {
      if (opts.queue.childElementCount < 2) return;
      const firstUserElement = opts.queue.firstElementChild as HTMLElement;
      const firstUserId = firstUserElement.id;
      const lastUserId = opts.queue.lastElementChild!.id;
      opts.styleOptions.forEach((styleOpts, i) => {
        const stacking = styleOpts.spaceBetweenElements < 0;
        const cssAttr = styleOpts.orientation === "horizontal" ? "left" : "top";
        const snapshotPosOfFirstEl = getCurrentValue(styleOpts, firstUserId);
        const snapPosOfLastEl = getCurrentValue(styleOpts, lastUserId);
        const declarations = new Map(
          styleOpts.stylesheet.get(firstUserId)!.declarations
        );
        if (stacking) {
          declarations.set(
            cssAttr,
            snapPosOfLastEl + -1 * styleOpts.spaceBetweenElements + "px"
          );
        } else {
          declarations.set(cssAttr, snapPosOfLastEl + "px");
        }
        styleOpts.stylesheet.put(firstUserId, declarations);
        if (styleOpts.media.matches) {
          if (stacking) {
            const moveToEndAndSomeAnimationKeyframe = {
              [cssAttr]: snapshotPosOfFirstEl + "px",
              offset: 0,
            };
            animate(firstUserElement, [moveToEndAndSomeAnimationKeyframe], {
              duration: 1000,
            }).finished.then(() => {
              firstUserElement.remove();
              opts.queue.appendChild(firstUserElement);
              rezindex();
              updateCurrentValue(styleOpts, firstUserId, snapPosOfLastEl);
              const moveToEndAnimationKeyframe = {
                [cssAttr]:
                  snapPosOfLastEl + styleOpts.spaceBetweenElements * -1 + "px",
                offset: 0,
              };
              animate(firstUserElement, [moveToEndAnimationKeyframe], {
                duration: 1000,
              });
            });
          } else {
            const moveToEndAnimationKeyframe = {
              [cssAttr]: snapshotPosOfFirstEl!,
              offset: 0,
            };
            animate(firstUserElement, [moveToEndAnimationKeyframe], {
              duration: 1000,
            }).finished.then(() => {
              const el = opts.queue.firstElementChild as HTMLElement;
              el.remove();
              opts.queue.appendChild(el);
              rezindex();
            });
          }
          moveElementsOneUpAnimation(1, opts.queue.childElementCount);
        }
      });
    },
  };
};
