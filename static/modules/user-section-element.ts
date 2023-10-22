import { RestClient } from "./rest-client";
import { SvgFactory } from "./svg";
import { RoomMessageListener } from "./room-message-listener";
import { ServerWelcomeCommand, CountingDownEvent, UserEnterEvent, UserLeftEvent } from "./interface";

export class UserQueueElement {

  private userSectionEl: HTMLElement;
  private roomMessageListener: RoomMessageListener;

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

  }
  
  public addNewUserToLine(userId: string, userName: string, hexColor: string, isCreator: boolean = false): Promise<void> {

    return new Promise((resolve) => {

      // rezindex previous users
      for (let i=0; i < this.userSectionEl.childElementCount; i++) {
          const u = this.userSectionEl.children[i] as HTMLDivElement;
          u.style.zIndex = (parseInt(u.style.zIndex) + 1).toString();
      }

      const userEl = document.createElement('div');
      userEl.id = userId;
      userEl.classList.add('user');
      userEl.style.backgroundColor = hexColor;
      userEl.style.zIndex = '0';
      userEl.style.left = '2000px'; // wayyy off screen

      const targetPosition = this.userSectionEl.lastElementChild as HTMLDivElement !== null 
        ? parseInt((this.userSectionEl.lastElementChild as HTMLDivElement).style.left) + 50 + "px"
        : '0px';

      this.userSectionEl.appendChild(userEl);

      const joinLineAnimation = userEl.animate([
          {
              left: targetPosition,
          }
        ], 
        {
          duration: 1000,
          fill: 'forwards'
        });

        joinLineAnimation.finished.then(() => {
          joinLineAnimation.commitStyles();
          joinLineAnimation.cancel();
          resolve();
        });
    });

  }

  public removeUserFromLine(userId: string): Promise<void> {
    return new Promise((resolve) => {
      const user = document.getElementById(userId);
      if (!user) throw new Error('No user cell element found');

      const userIndex = Array.from(this.userSectionEl.children).findIndex((el) => el.id === userId);

      const leaveLineAnimation = user.animate([
          {
              left: '2000px',
          }
        ], 
        {
          duration: 1000,
          fill: 'forwards'
        });
        leaveLineAnimation.pause();

        const oneUpAnimations: Animation[] = [];
        for (let i = userIndex + 1; i < this.userSectionEl.childElementCount; i++) {
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
        leaveLineAnimation.play();
        oneUpAnimations.forEach(a => a.play());
        Promise.all([leaveLineAnimation.finished, ...oneUpAnimations.map(a => a.finished)])
          .then(() => {
              oneUpAnimations.forEach(a => {a.commitStyles(); a.cancel();});
              leaveLineAnimation.commitStyles();
              leaveLineAnimation.cancel();
              user.remove();

              for (let i=0,j=this.userSectionEl.childElementCount - 1; i < this.userSectionEl.childElementCount; i++,j--) {
                  const u = this.userSectionEl.children[i] as HTMLDivElement;
                  console.log(j);
                  u.style.zIndex = j.toString();
              }

              resolve();

          });
      });


  }

  private removeAndPlaceFirstUserAtEndOfLine() {
    const firstUserEl = this.userSectionEl.firstElementChild;
    if (!firstUserEl) throw new Error('No first user element found');
    this.userSectionEl.removeChild(firstUserEl);
    this.userSectionEl.appendChild(firstUserEl);
  }

  // Should be run on nextInQueue event
  private nextInLine() {
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
            left: parseInt(lastPos.left) + 100 + 'px',
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
