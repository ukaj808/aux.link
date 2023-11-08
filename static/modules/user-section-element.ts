import { RestClient } from "./rest-client";
import { RoomMessageListener } from "./room-message-listener";
import {
  ServerWelcomeCommand,
  CountingDownEvent,
  UserEnterEvent,
  UserLeftEvent,
} from "./interface";
import { MutableStyleSheet } from "./stylesheet-manipulations";

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
  private mediaQueryList: MediaQueryList;
  private runningAnimations: Animation[] = [];

  constructor(
    roomMessageListener: RoomMessageListener,
    restClient: RestClient,
    mobileStyleSheet: MutableStyleSheet,
    desktopStyleSheet: MutableStyleSheet
  ) {
    const userSectionEl = document.getElementById("user-section");
    if (!userSectionEl) throw new Error("No user section element found");
    this.userSectionEl = userSectionEl;

    this.mobileStyleSheet = mobileStyleSheet;
    this.desktopStyleSheet = desktopStyleSheet;

    this.roomMessageListener = roomMessageListener;
    this.roomMessageListener.subscribe("ServerWelcomeCommand", (data) => {
      const welcomeCommand = data as ServerWelcomeCommand;
      this.addNewUserToLine(
        welcomeCommand.userId,
        welcomeCommand.userName,
        welcomeCommand.hexColor
      );
    });
    this.roomMessageListener.subscribe("UserEnterEvent", (data) => {
      const userEnterEvent = data as UserEnterEvent;
      this.addNewUserToLine(
        userEnterEvent.userId,
        userEnterEvent.userName,
        userEnterEvent.hexColor
      );
    });
    this.roomMessageListener.subscribe("UserLeftEvent", (data) => {
      const userLeftEvent = data as UserLeftEvent;
      this.removeUserFromLine(userLeftEvent.userId);
    });
    this.roomMessageListener.subscribe("NextInQueueEvent", (data) => {
      // this.nextInLine();
    });

    this.mediaQueryList = window.matchMedia(
      "screen and (min-width:0px) and (max-width:1025px)"
    );
    this.mediaQueryList.addEventListener("change", (e) => {
      this.runningAnimations.forEach((a) => a.finish());
    });

    // Pre-process server side rendered user elements and there stylesheet injections
    Array.from(this.userSectionEl.children).forEach((u) => {
      const userEl = u as HTMLDivElement;

      const mobileZIndex = userEl.getAttribute("data-mobile-z-index");
      if (!mobileZIndex) throw new Error("No zIndex attribute found");

      const mobileLeft = userEl.getAttribute("data-mobile-left");
      if (!mobileLeft) throw new Error("No left attribute found");

      const desktopTop = userEl.getAttribute("data-desktop-top");
      if (!desktopTop) throw new Error("No top attribute found");

      const backgroundColor = userEl.getAttribute("data-hex-color");
      if (!backgroundColor)
        throw new Error("No backgroundColor attribute found");

      this.mobileStyleSheet.put(
        userEl.id,
        new Map([
          ["z-index", mobileZIndex],
          ["left", mobileLeft],
        ])
      );
      this.desktopStyleSheet.put(userEl.id, new Map([["top", desktopTop]]));

      // static style... storing in style attribute to indicate that
      userEl.style.backgroundColor = backgroundColor;
    });

    this.endPositionPx = this.userSectionEl.childElementCount * this.offset;

    const stateAttribute = userSectionEl.getAttribute("data-og-state");
    if (!stateAttribute) throw new Error("No state attribute found");
  }

  public addNewUserToLine(userId: string, userName: string, hexColor: string) {
    // Rezindex all users in mobile
    for (
      let i = 0, j = this.userSectionEl.childElementCount;
      i < this.userSectionEl.childElementCount;
      i++, j--
    ) {
      const userEl = this.userSectionEl.children[i] as HTMLDivElement;
      const idRule = this.mobileStyleSheet.get(userEl.id);
      if (!idRule) throw new Error("No rule found");
      // declarations are read only, so we need to create a new map
      const updatedDeclarations = new Map(
        Array.from(idRule.declarations).map(([k, v]) => {
          if (k === "z-index") {
            return [k, j.toString()];
          }
          return [k, v];
        })
      );
      this.mobileStyleSheet.put(userEl.id, updatedDeclarations);
    }

    const userEl = document.createElement("div");
    userEl.id = userId;
    userEl.style.backgroundColor = hexColor;
    const declarations = new Map<string, string>([
      ["z-index", "0"],
      ["left", this.endPositionPx + (this.endPositionPx == 0 ? "" : "px")],
    ]);
    this.mobileStyleSheet.put(userId, declarations);
    userEl.classList.add("user");

    this.userSectionEl.appendChild(userEl);

    const enterAnimation = userEl.animate(
      [
        {
          left: "1000px",
          offset: 0,
        },
      ],
      {
        duration: 10000,
      }
    );
    enterAnimation.finished.then(() => {
      this.runningAnimations = this.runningAnimations.filter(
        (a) => a !== enterAnimation
      );
    });
    this.runningAnimations.push(enterAnimation);

    this.endPositionPx += this.offset;
  }

  public removeUserFromLine(userId: string) {
    const user = document.getElementById(userId);
    if (!user) throw new Error("No user cell element found");

    const userIndex = Array.from(this.userSectionEl.children).findIndex(
      (el) => el.id === userId
    );

    const leaveAnimation = user.animate(
      [
        {
          left: "1000px",
        },
      ],
      {
        duration: 1000,
      }
    );
    leaveAnimation.finished.then(() => {
      this.mobileStyleSheet.delete(userId);
      user.remove();
      this.runningAnimations = this.runningAnimations.filter(
        (a) => a !== leaveAnimation
      );
    });
    this.runningAnimations.push(leaveAnimation);

    for (
      let i = 0, j = this.userSectionEl.childElementCount - 1;
      i < this.userSectionEl.childElementCount;
      i++, j--
    ) {
      const u = this.userSectionEl.children[i] as HTMLDivElement;
      const rule = this.mobileStyleSheet.get(u.id);
      if (!rule) throw new Error("No rule found");
      const prevLeft = Array.from(rule.declarations).find(([k, v]) => {
        return k === "left";
      })?.[1];
      if (!prevLeft) throw new Error("No left declaration found");
      const newDeclaration: Map<string, string> =
        i <= userIndex
          ? new Map([
              ["z-index", j.toString()],
              ["left", prevLeft],
            ])
          : new Map([
              ["z-index", j.toString()],
              ["left", parseInt(prevLeft) - this.offset + "px"],
            ]);

      this.mobileStyleSheet.put(u.id, newDeclaration);

      const moveUpAnimation = u.animate(
        [
          {
            left: prevLeft,
            offset: 0,
          },
        ],
        {
          duration: 1000,
        }
      );
      this.runningAnimations.push(moveUpAnimation);
      moveUpAnimation.finished.then(() => {
        this.runningAnimations = this.runningAnimations.filter(
          (a) => a !== moveUpAnimation
        );
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