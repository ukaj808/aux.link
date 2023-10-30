import { RestClient } from "./rest-client";
import { SvgFactory } from "./svg";
import { RoomMessageListener } from "./room-message-listener";
import { ServerWelcomeCommand, CountingDownEvent, UserEnterEvent, UserLeftEvent } from "./interface";
import { MutableStyleSheet } from "./stylesheet-manipulations";
import { Declaration, Rule } from "css";

// Animations in this class are based on the following recommendations from the W3C for
// achieving animations whose effects should b applied indefinitely:
// https://drafts.csswg.org/web-animations-1/#example-0307c584
export class UserQueueElement {

  private userSectionEl: HTMLElement;
  private mobileStyleSheet: MutableStyleSheet;
  private desktopStyleSheet: MutableStyleSheet;
  private roomMessageListener: RoomMessageListener;
  private offset = 20;
  private endPositionPx: number;

  constructor(roomMessageListener: RoomMessageListener, restClient: RestClient, mobileStyleSheet: MutableStyleSheet, desktopStyleSheet: MutableStyleSheet) {

    const userSectionEl = document.getElementById("user-section");
    if (!userSectionEl) throw new Error('No user section element found');
    this.userSectionEl = userSectionEl;

    this.mobileStyleSheet = mobileStyleSheet;
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
      // this.nextInLine();
    });

    // Pre-process server side rendered user elements and there stylesheet injections
    Array.from(this.userSectionEl.children).forEach(u => {
      const userEl = u as HTMLDivElement;

      const mobileZIndex = userEl.getAttribute('data-mobile-z-index');
      if (!mobileZIndex) throw new Error('No zIndex attribute found');

      const left = userEl.getAttribute('data-mobile-left');
      if (!left) throw new Error('No left attribute found');

      const backgroundColor = userEl.getAttribute('data-hex-color');
      if (!backgroundColor) throw new Error('No backgroundColor attribute found');

      const mobileRule: Rule = {
        selectors: ['#'+userEl.id],
        declarations: [
          {
            property: 'z-index',
            value: mobileZIndex,
          },
          {
            property: 'left',
            value: left,
          },
        ],
      } 

      this.mobileStyleSheet.put(userEl.id, mobileRule);

      // static style... storing in style attribute to indicate that
      userEl.style.backgroundColor = backgroundColor;
    });

    this.endPositionPx = this.userSectionEl.childElementCount * this.offset;

    const stateAttribute = userSectionEl.getAttribute('data-og-state');
    if (!stateAttribute) throw new Error('No state attribute found');

  }

  public addNewUserToLine(userId: string, userName: string, hexColor: string) {


    // todo: rezindex previous users
    Array.from(this.userSectionEl.children).forEach((u, i) => {
      const userEl = u as HTMLDivElement;
      const rule = this.mobileStyleSheet.get(userEl.id);
      if (!rule) throw new Error('No rule found');
      rule.declarations?.forEach((d, i) => {
        const dec = d as Declaration;
        if (dec.property === 'z-index') {
          dec.value = i.toString();
        }
      });
    });

    const userEl = document.createElement('div');
    userEl.id = userId;
    userEl.style.backgroundColor = hexColor;
    this.mobileStyleSheet.put(userId, {
      selectors: ['#'+userId],
      declarations: [
        {
          property: 'z-index',
          value: '0',
        },
        {
          property: 'left',
          value: this.endPositionPx + (this.endPositionPx == 0 ? '' : 'px'),
        },
      ],
    });
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
        this.mobileStyleSheet.delete(userId);
        user.remove();
      });

    for (let i = 0, j = this.userSectionEl.childElementCount - 1; i < this.userSectionEl.childElementCount; i++, j--) {
      const u = this.userSectionEl.children[i] as HTMLDivElement;
      const rule = this.mobileStyleSheet.get(u.id);
      if (!rule) throw new Error('No rule found');
      const prevLeft = rule.declarations?.find((d) => {
        const dec = d as Declaration;
        return dec.property === 'left';
      });
      if (!prevLeft) throw new Error('No left declaration found');
      const newMobileRule: Rule = i <= userIndex ?
        { 
          selectors: ['#'+u.id],
          declarations: [
            {
              property: 'z-index',
              value: j.toString(),
            },
            {
              property: 'left',
              value: (prevLeft as Declaration).value,
            },
          ],
        } :
        {
          selectors: ['#'+u.id],
          declarations: [
            {
              property: 'z-index',
              value: j.toString(),
            },
            {
              property: 'left',
              value: parseInt((prevLeft as Declaration).value as string) - this.offset + 'px',
            },
          ],
        };

        this.mobileStyleSheet.put(u.id, newMobileRule);

        u.animate([
          {
            left: (prevLeft as Declaration).value,
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
  /*
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
      const user = document.getElementById(userId);
      if (!user) throw new Error('No user element found');
  
      const mobileRule = `#${userId} { z-index: ${position.zIndex}; left: ${position.left}; }`;
      const mobileRuleIndex = this.mobileStyleSheet.cssRules.length;
  
      this.mobileStyleSheet.insertRule(mobileRule, mobileRuleIndex);
      user.setAttribute('data-mobile-rule-index', mobileRuleIndex.toString());
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
  */

}