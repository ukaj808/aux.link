import Flickity from "flickity";
import { UserElement, UserElementFactory } from "./user-element";
import { RestClient } from "./rest-client";
import { SvgFactory } from "./svg";
import { RoomMessageListener } from "./room-message-listener";

export class OrderElement {

  private el: HTMLElement;
  private roomMessageListener: RoomMessageListener;
  private orderCarouselEl: Element;
  private userElementFactory: UserElementFactory;
  private flkty: Flickity;

  constructor(roomMessageListener: RoomMessageListener, restClient: RestClient, svgFactory: SvgFactory) {

    const el = document.getElementById("order");
    if (!el) throw new Error('No order element found');
    this.el = el;

    const optOrderCarouselEl = document.querySelector(".user-carousel");
    if (!optOrderCarouselEl) throw new Error('No order carousel element found');
    this.orderCarouselEl = optOrderCarouselEl;

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

    const stateAttribute = el.getAttribute('data-state');
    if (!stateAttribute) throw new Error('No state attribute found');
    console.log('stateAttribute', stateAttribute);
    console.log(JSON.parse(stateAttribute));

    this.userElementFactory = new UserElementFactory(restClient, svgFactory, this.orderCarouselEl);
    this.flkty = new Flickity( this.orderCarouselEl, {
      // options
      cellAlign: 'left',
      prevNextButtons: false,
      pageDots: false,
      contain: true
    });
  }

  public addThisUserToOrderCarousel(userId: string, userName: string, isCreator: boolean = false) {
    this.addNewUserToOrderCarousel(userId, userName, isCreator);
  }
  
  public addNewUserToOrderCarousel(userId: string, userName: string, isCreator: boolean = false) {
    const userEl = this.userElementFactory.createNewUser(userId, userName, isCreator);
    this.flkty.append(userEl.getEl());
  }

  public removeUserFromOrderCarousel(userId: string) {
    const optUserCellEl = document.getElementById(userId);
    if (!optUserCellEl) throw new Error('No user cell element found');
    this.userElementFactory.removeUser(userId);
    this.flkty.remove(optUserCellEl);
  }

}
