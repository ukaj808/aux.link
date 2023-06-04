import Flickity from "flickity";
import { UserElement, UserElementFactory } from "./user-element";
import { RestClient } from "./rest-client";
import { SvgFactory } from "./svg";

export class OrderElement {

  private orderCarouselEl: Element;
  private userElementFactory: UserElementFactory;
  private flkty: Flickity;

  constructor(restClient: RestClient, svgFactory: SvgFactory) {
    const optOrderCarouselEl = document.querySelector(".user-carousel");
    if (!optOrderCarouselEl) throw new Error('No order carousel element found');
    this.orderCarouselEl = optOrderCarouselEl;
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
