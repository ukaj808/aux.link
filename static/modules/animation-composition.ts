type ForwardAnimationCompositionStep = { element: HTMLElement, order: number };
type ForwardAnimationComposition = { run: () => Promise<void> };

function animationCompositionStep(element: HTMLElement, order: number): ForwardAnimationCompositionStep {
    return {

    }
}

function animationComposition(steps: ForwardAnimationCompositionStep[]): ForwardAnimationComposition {
    return {
        run: async () => {}
    }
}
