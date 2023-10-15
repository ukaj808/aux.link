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
      this.addNewUserToOrderCarousel(welcomeCommand.userId, welcomeCommand.userName, welcomeCommand.hexColor, welcomeCommand.isCreator);

    });
    this.roomMessageListener.subscribe('UserEnterEvent', (data) => {
      const userEnterEvent = data as UserEnterEvent;
      this.addNewUserToOrderCarousel(userEnterEvent.userId, userEnterEvent.userName, userEnterEvent.hexColor);
    });
    this.roomMessageListener.subscribe('UserLeftEvent', (data) => {
      const userLeftEvent = data as UserLeftEvent;
      this.removeUserFromOrderCarousel(userLeftEvent.userId);
    });
    this.roomMessageListener.subscribe('NextInQueueEvent', (data) => {
      console.log('NextInQueueEvent');
      this.removeAndPlaceFirstUserAtEndOfQueue();
    });

    const stateAttribute = userSectionEl.getAttribute('data-og-state');
    if (!stateAttribute) throw new Error('No state attribute found');

    this.userElementFactory = new UserElementFactory(restClient, svgFactory, this.userSectionEl);
  }

  public addThisUserToOrderCarousel(userId: string, userName: string, hexColor: string, isCreator: boolean = false) {
    this.addNewUserToOrderCarousel(userId, userName, hexColor, isCreator);
  }
  
  public addNewUserToOrderCarousel(userId: string, userName: string, hexColor: string, isCreator: boolean = false) {
    const userEl = this.userElementFactory.createNewUser(userId, userName, hexColor, isCreator);
    this.userSectionEl.appendChild(userEl.getEl());
  }

  public removeUserFromOrderCarousel(userId: string) {
    const optUserCellEl = document.getElementById(userId);
    if (!optUserCellEl) throw new Error('No user cell element found');
    this.userSectionEl.removeChild(optUserCellEl);
  }

  private removeAndPlaceFirstUserAtEndOfQueue() {
    const firstUserEl = this.userSectionEl.firstElementChild;
    if (!firstUserEl) throw new Error('No first user element found');
    this.userSectionEl.removeChild(firstUserEl);
    this.userSectionEl.appendChild(firstUserEl);
  }

  private staggerUsers = (node, zIndex) => {
    if (node === null) return;
    node.style.marginLeft = -100;
    node.style.zIndex = zIndex;
    const nextZIndex = zIndex - 1;
    this.staggerUsers(node.nextElementSibling, nextZIndex);
  }
  
  private runNextInLineAnimation = () => {
      if (this.userSectionEl.childElementCount < 2) return;
  
      const lastPos = { 
          marginLeft: usersList.children[usersList.childElementCount-1].style.marginLeft,
          zIndex: usersList.children[usersList.childElementCount-1].style.zIndex
      }
  
      const moveToEndAndSomeAnimation = usersList.children[0].animate([
          {
              marginLeft: parseInt(lastPos.marginLeft) + 250 + 'px',
          }
      ], 
      {
          duration: 1000,
          fill: 'forwards'
      });
      moveToEndAndSomeAnimation.pause();
  
      const moveToLastPlaceAnimation = usersList.children[0].animate([
          {
              marginLeft: lastPos.marginLeft,
          }
      ], 
      {
          duration: 1000,
          fill: 'forwards'
      });
      moveToLastPlaceAnimation.pause();
  
      const oneUpAnimations = [];
      for (let i = 1; i < usersList.childElementCount; i++) {
          const u1 = usersList.children[i];
          const u2 = usersList.children[i-1]; // User ahead one position
          const newPos = {
              marginLeft: u2.style.marginLeft,
              zIndex: u2.style.zIndex
          };
          const upOneAnimation = u1.animate([
              {
                  marginLeft: newPos.marginLeft,
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
              const head = usersList.children[0];
  
              // at this point, the left property should have been updated through the commitStyles() call
              // but we still need to update the zIndex
              for (let i=1; i < usersList.childElementCount; i++) {
                  const u = usersList.children[i];
                  u.style.zIndex = parseInt(u.style.zIndex) + 1;
              }
  
              head.remove();
              usersList.appendChild(head);
              head.style.zIndex = 0;
  
              moveToLastPlaceAnimation.play();
              moveToLastPlaceAnimation.finished.then(() => {
                  moveToLastPlaceAnimation.commitStyles();
                  moveToLastPlaceAnimation.cancel();
              });
          });
  };
  
  }

}
