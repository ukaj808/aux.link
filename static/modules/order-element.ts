import * as Flickity from 'flickity';

export class OrderElement {

  private orderCarouselEl: HTMLElement;
  private flkty: Flickity;

  constructor() {
    this.orderCarouselEl = document.querySelector(".user-carousel");
    this.flkty = new Flickity( this.orderCarouselEl, {
      // options
      cellAlign: 'left',
      prevNextButtons: false,
      pageDots: false,
      contain: true
    });
  }
  
  addNewUserToOrderCarousel(userId, userName) {
      const userCellEl = document.createElement('div');
      userCellEl.id = userId;
      userCellEl.className = 'user-carousel-cell';
      this.flkty.append(userCellEl);
  }

  removeUserFromOrderCarousel(userId) {
    const userCellEl = document.getElementById(userId);
    this.flkty.remove(userCellEl);
  }

}
