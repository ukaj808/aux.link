import { MutableStyleSheet } from "./stylesheet-manipulations";

type ResponsiveQueueAnimationManager = () => {
    enter: (id: string) => void;
    leave: (id: string) => void;
    cycle: () => void;  
};

function responsiveQueueAnimationManager(stylesheets: MutableStyleSheet[], queue: HTMLElement): ResponsiveQueueAnimationManager {
}