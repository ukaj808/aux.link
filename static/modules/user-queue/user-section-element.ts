import { UserElementFactory } from "./user-element";
import { RestClient } from "../rest-client";
import { SvgFactory } from "../svg";
import { RoomMessageListener } from "../room-message-listener";
import { ServerWelcomeCommand, CountingDownEvent, UserEnterEvent, UserLeftEvent } from "../interface";

export class UserQueueElement {

  private userSectionEl: HTMLElement;
  private roomMessageListener: RoomMessageListener;
  private userElementFactory: UserElementFactory;

  constructor(roomMessageListener: RoomMessageListener, restClient: RestClient, svgFactory: SvgFactory) {

    const userSectionEl = document.getElementById("user-section");
    if (!userSectionEl) throw new Error('No user section element found');
    this.userSectionEl = userSectionEl;

    this.roomMessageListener = roomMessageListener;
    this.roomMessageListener.subscribe('ServerWelcomeCommand', (data) => {
      const welcomeCommand = data as ServerWelcomeCommand;
      this.addNewUserToLine(welcomeCommand.userId, welcomeCommand.userName, welcomeCommand.hexColor, welcomeCommand.isCreator);

    });
    this.roomMessageListener.subscribe('UserEnterEvent', (data) => {
      const userEnterEvent = data as UserEnterEvent;
      this.addNewUserToLine(userEnterEvent.userId, userEnterEvent.userName, userEnterEvent.hexColor);
    });
    this.roomMessageListener.subscribe('UserLeftEvent', (data) => {
      const userLeftEvent = data as UserLeftEvent;
      this.removeUserFromLine(userLeftEvent.userId);
    });
    this.roomMessageListener.subscribe('NextInQueueEvent', (data) => {
      console.log('NextInQueueEvent');
      this.removeAndPlaceFirstUserAtEndOfLine();
    });

    const stateAttribute = userSectionEl.getAttribute('data-og-state');
    if (!stateAttribute) throw new Error('No state attribute found');

    this.userElementFactory = new UserElementFactory(restClient, svgFactory, this.userSectionEl);

    this.staggerUsers(this.userSectionEl.firstElementChild as HTMLDivElement, 0, this.userSectionEl.childElementCount);
  }

  
  public addNewUserToLine(userId: string, userName: string, hexColor: string, isCreator: boolean = false) {
    const userEl = this.userElementFactory.createNewUser(userId, userName, hexColor, isCreator);
    this.userSectionEl.appendChild(userEl.getEl());
  }

  public removeUserFromLine(userId: string) {
    const optUserCellEl = document.getElementById(userId);
    if (!optUserCellEl) throw new Error('No user cell element found');
    this.userSectionEl.removeChild(optUserCellEl);
  }

  private removeAndPlaceFirstUserAtEndOfLine() {
    const firstUserEl = this.userSectionEl.firstElementChild;
    if (!firstUserEl) throw new Error('No first user element found');
    this.userSectionEl.removeChild(firstUserEl);
    this.userSectionEl.appendChild(firstUserEl);
  }

  // Should only be run once in construction; staggering the users already in the room before you
  private staggerUsers = (node: HTMLDivElement | null, offset: number, zIndex: number) => {
    if (node === null) return;
    const calcOffset = 8 / this.userSectionEl.childElementCount;
    node.style.left = offset.toString() + 'rem';
    node.style.zIndex = zIndex.toString();
    const nextOffset = offset + calcOffset;
    const nextZIndex = zIndex - 1;
    this.staggerUsers(node.nextElementSibling as HTMLDivElement, nextOffset, nextZIndex);
  };


  private addNewUserToLineAnimation = (userEl: HTMLDivElement) =>{
  // todo: Should be run on userEnter event; moving a element from outside the window, into the last position
  };

  private removeUserFromLineAnimation = (userEl: HTMLDivElement) => {
    // todo: Should be run on userLeft event; moving an element outside the window and moving up the rest of the elements
  };


  // Should be run on nextInQueue event
  private runNextInLineAnimation = () => {
    if (this.userSectionEl.childElementCount < 2) return;
    if (this.userSectionEl.firstElementChild === null) return;
    if (this.userSectionEl.lastElementChild === null) return;

    const lastEl = this.userSectionEl.lastElementChild as HTMLDivElement;
    const lastPos = { 
        left: lastEl.style.left,
        zIndex: lastEl.style.zIndex
    }

    const moveToEndAndSomeAnimation = this.userSectionEl.firstElementChild.animate([
        {
            left: parseInt(lastPos.left) + 8 + 'rem',
        }
    ], 
    {
        duration: 1000,
        fill: 'forwards'
    });
    moveToEndAndSomeAnimation.pause();

    const moveToLastPlaceAnimation = this.userSectionEl.firstElementChild.animate([
        {
            left: lastPos.left,
        }
    ], 
    {
        duration: 1000,
        fill: 'forwards'
    });
    moveToLastPlaceAnimation.pause();

    const oneUpAnimations: Animation[] = [];
    for (let i = 1; i < this.userSectionEl.childElementCount; i++) {
        const u1 = this.userSectionEl.children[i] as HTMLDivElement;
        const u2 = this.userSectionEl.children[i-1] as HTMLDivElement; // User ahead one position
        const newPos = {
            left: u2.style.left,
            zIndex: u2.style.zIndex
        };
        const upOneAnimation = u1.animate([
            {
                left: newPos.left,
            }
        ], 
        {
            duration: 1000,
            fill: 'forwards'
        });
        upOneAnimation.pause();
        oneUpAnimations.push(upOneAnimation);
    }

    // Play all animations
    moveToEndAndSomeAnimation.play();
    oneUpAnimations.forEach(a => a.play());
    Promise.all([moveToEndAndSomeAnimation.finished, ...oneUpAnimations.map(a => a.finished)])
        .then(() => {

            oneUpAnimations.forEach(a => {a.commitStyles(); a.cancel();});
            moveToEndAndSomeAnimation.commitStyles();
            moveToEndAndSomeAnimation.cancel();
            const head = this.userSectionEl.firstElementChild as HTMLDivElement;

            // at this point, the left property should have been updated through the commitStyles() call
            // but we still need to update the zIndex
            for (let i=1; i < this.userSectionEl.childElementCount; i++) {
                const u = this.userSectionEl.children[i] as HTMLDivElement;
                u.style.zIndex = (parseInt(u.style.zIndex) + 1).toString();
            }

            head.remove();
            this.userSectionEl.appendChild(head);
            head.style.zIndex = '0';

            moveToLastPlaceAnimation.play();
            moveToLastPlaceAnimation.finished.then(() => {
                moveToLastPlaceAnimation.commitStyles();
                moveToLastPlaceAnimation.cancel();
            });
        });
};

}
