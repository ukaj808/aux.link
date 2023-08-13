import { UserElementFactory } from "./user-element";
import { RestClient } from "./rest-client";
import { SvgFactory } from "./svg";
import { RoomMessageListener } from "./room-message-listener";
import { ServerWelcomeCommand, UserEnterEvent, UserLeftEvent } from "./interface";

export class OrderElement {

  private el: HTMLElement;
  private roomMessageListener: RoomMessageListener;
  private userQueueEl: HTMLOListElement;
  private userElementFactory: UserElementFactory;

  constructor(roomMessageListener: RoomMessageListener, restClient: RestClient, svgFactory: SvgFactory) {

    const el = document.getElementById("order");
    if (!el) throw new Error('No order element found');
    this.el = el;

    const userQueueEl = document.getElementById("user-queue");
    if (!userQueueEl) throw new Error('No user queue element found');
    this.userQueueEl = userQueueEl as HTMLOListElement;

    this.roomMessageListener = roomMessageListener;
    this.roomMessageListener.subscribe('ServerWelcomeCommand', (data) => {
      const welcomeCommand = data as ServerWelcomeCommand;
      this.addNewUserToOrderCarousel(welcomeCommand.userId, welcomeCommand.userName, welcomeCommand.isCreator);

    });
    this.roomMessageListener.subscribe('UserEnterEvent', (data) => {
      const userEnterEvent = data as UserEnterEvent;
      this.addNewUserToOrderCarousel(userEnterEvent.userId, userEnterEvent.userName);
    });
    this.roomMessageListener.subscribe('UserLeftEvent', (data) => {
      const userLeftEvent = data as UserLeftEvent;
      this.removeUserFromOrderCarousel(userLeftEvent.userId);
    });

    const stateAttribute = el.getAttribute('data-og-state');
    if (!stateAttribute) throw new Error('No state attribute found');

    this.userElementFactory = new UserElementFactory(restClient, svgFactory, this.userQueueEl);
  }

  public addThisUserToOrderCarousel(userId: string, userName: string, isCreator: boolean = false) {
    this.addNewUserToOrderCarousel(userId, userName, isCreator);
  }
  
  public addNewUserToOrderCarousel(userId: string, userName: string, isCreator: boolean = false) {
    const userEl = this.userElementFactory.createNewUser(userId, userName, isCreator);
    this.userQueueEl.appendChild(userEl.getEl());
  }

  public removeUserFromOrderCarousel(userId: string) {
    const optUserCellEl = document.getElementById(userId);
    if (!optUserCellEl) throw new Error('No user cell element found');
    this.userElementFactory.removeUser(userId);
    this.userQueueEl.removeChild(optUserCellEl);
  }

}
