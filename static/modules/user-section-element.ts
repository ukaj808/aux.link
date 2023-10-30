import { RestClient } from "./rest-client";
import { SvgFactory } from "./svg";
import { RoomMessageListener } from "./room-message-listener";
import { ServerWelcomeCommand, CountingDownEvent, UserEnterEvent, UserLeftEvent } from "./interface";

// Animations in this class are based on the following recommendations from the W3C for
// achieving animations whose effects should b applied indefinitely:
// https://drafts.csswg.org/web-animations-1/#example-0307c584
export class UserQueueElement {

  private userSectionEl: HTMLElement;
  private mobileStyleSheet: CSSStyleSheet;
  private desktopStyleSheet: CSSStyleSheet;
  private roomMessageListener: RoomMessageListener;
  private offset = 20;
  private endPositionPx: number;

  constructor(roomMessageListener: RoomMessageListener, restClient: RestClient, svgFactory: SvgFactory) {

    const userSectionEl = document.getElementById("user-section");
    if (!userSectionEl) throw new Error('No user section element found');
    this.userSectionEl = userSectionEl;

    const mobileStyleSheet = Array.from(document.styleSheets).find((s) => (s.ownerNode as HTMLLinkElement)?.id === 'room-mobile-stylesheet');
    if (!mobileStyleSheet) throw new Error('No mobile stylesheet element found');
    this.mobileStyleSheet = mobileStyleSheet;

    const desktopStyleSheet = Array.from(document.styleSheets).find((s) => (s.ownerNode as HTMLLinkElement)?.id === 'room-desktop-stylesheet');
    if (!desktopStyleSheet) throw new Error('No desktop stylesheet element found');
    this.desktopStyleSheet = desktopStyleSheet;

    this.roomMessageListener = roomMessageListener;
    this.roomMessageListener.subscribe('ServerWelcomeCommand', (data) => {
      const welcomeCommand = data as ServerWelcomeCommand;
      this.addNewUserToLine(welcomeCommand.userId, welcomeCommand.userName, welcomeCommand.hexColor);
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
      this.nextInLine();
    });

    this.endPositionPx = this.userSectionEl.childElementCount * this.offset;

    const stateAttribute = userSectionEl.getAttribute('data-og-state');
    if (!stateAttribute) throw new Error('No state attribute found');

  }

  public addNewUserToLine(userId: string, userName: string, hexColor: string) {


    // rezindex previous users
    for (let i = 0; i < this.userSectionEl.childElementCount; i++) {
      const u = this.userSectionEl.children[i] as HTMLDivElement;
      u.style.zIndex = (parseInt(u.style.zIndex) + 1).toString();
    }

    const userEl = document.createElement('div');
    userEl.id = userId;
    userEl.style.backgroundColor = hexColor;
    const userMobileRule = `#${userId} { z-index: 0; left: ${this.endPositionPx + (this.endPositionPx == 0 ? '' : 'px')}; }`;
    const mobileRuleIndex = this.mobileStyleSheet.cssRules.length;
    this.mobileStyleSheet.insertRule(userMobileRule, mobileRuleIndex);
    userEl.setAttribute('data-mobile-rule-index', mobileRuleIndex.toString());
    userEl.classList.add('user');

    this.userSectionEl.appendChild(userEl);

    userEl.animate([
      {
        left: '1000px',
        offset: 0,
      }
    ],
      {
        duration: 1000,
      });

    this.endPositionPx += this.offset;

  }

  public removeUserFromLine(userId: string) {
    const user = document.getElementById(userId);
    if (!user) throw new Error('No user cell element found');

    const userIndex = Array.from(this.userSectionEl.children).findIndex((el) => el.id === userId);

    user.animate([
      {
        left: '1000px',
      }
    ],
      {
        duration: 1000,
      }).finished.then(() => { 
        this.mobileStyleSheet.deleteRule(parseInt(user.getAttribute('data-mobile-rule-index') as string));
        user.remove();
      });

    for (let i = 0, j = this.userSectionEl.childElementCount - 1; i < this.userSectionEl.childElementCount; i++, j--) {
      const u = this.userSectionEl.children[i] as HTMLDivElement;
      const mobileRuleIndex = parseInt(u.getAttribute('data-mobile-rule-index') as string);
      const mobileRule = this.mobileStyleSheet.cssRules[mobileRuleIndex] as CSSStyleRule;
      const prevLeft = mobileRule.style.left;
      const newMobileRule = i <= userIndex ?
        `#${u.id} { z-index: ${j.toString()}; left: ${prevLeft}; }` :
        `#${u.id} { z-index: ${j.toString()}; left: ${parseInt(prevLeft) - this.offset + 'px'}; }`;
      

      const newMobileRuleIndex = this.mobileStyleSheet.cssRules.length;
      this.mobileStyleSheet.deleteRule(mobileRuleIndex);
      this.mobileStyleSheet.insertRule(newMobileRule, newMobileRuleIndex);
      u.setAttribute('data-mobile-rule-index', newMobileRuleIndex.toString());

        u.animate([
          {
            left: prevLeft,
            offset: 0,
          }
        ],
          {
            duration: 1000,
          });
    }

    this.endPositionPx -= this.offset;
  }

  // Should be run on nextInQueue event
  private nextInLine() {
    if (this.userSectionEl.childElementCount < 2) return;
    if (this.userSectionEl.firstElementChild === null) return;
    if (this.userSectionEl.lastElementChild === null) return;

    const moveToEndAndSomeAnimation = this.userSectionEl.firstElementChild.animate([
      {
        left: this.endPositionPx + 100 + 'px',
      }
    ],
      {
        duration: 1000,
        fill: 'forwards'
      });
    moveToEndAndSomeAnimation.pause();

    const moveToLastPlaceAnimation = this.userSectionEl.firstElementChild.animate([
      {
        left: this.endPositionPx + 'px',
      }
    ],
      {
        duration: 1000,
        fill: 'forwards'
      });
    moveToLastPlaceAnimation.pause();

    const oneUpAnimations: Animation[] = [];
    for (let i = 1; i < this.userSectionEl.childElementCount; i++) {
      const u = this.userSectionEl.children[i] as HTMLDivElement;
      const upOneAnimation = u.animate([
        {
          left: parseInt(u.style.left) - this.offset + 'px',
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

        oneUpAnimations.forEach(a => { a.commitStyles(); a.cancel(); });
        moveToEndAndSomeAnimation.commitStyles();
        moveToEndAndSomeAnimation.cancel();
        const head = this.userSectionEl.firstElementChild as HTMLDivElement;

        // at this point, the left property should have been updated through the commitStyles() call
        // but we still need to update the zIndex
        for (let i = 1; i < this.userSectionEl.childElementCount; i++) {
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

  private deleteRuleFromStyleSheet(stylesheet: CSSStyleSheet, userId: string) {
    Array.from(stylesheet.cssRules).forEach((rule, index) => {
      if ((rule as CSSStyleRule).selectorText === `#${userId}`) {
        stylesheet.deleteRule(index);
      }
    });
  }

}