import Flickity from 'flickity';

export class OrderElement {

  private orderCarouselEl: Element;
  private flkty: Flickity;

  constructor() {
    const optOrderCarouselEl = document.querySelector(".user-carousel");
    if (!optOrderCarouselEl) throw new Error('No order carousel element found');
    this.orderCarouselEl = optOrderCarouselEl;;
    this.flkty = new Flickity( this.orderCarouselEl, {
      // options
      cellAlign: 'left',
      prevNextButtons: false,
      pageDots: false,
      contain: true
    });
  }
  
  addNewUserToOrderCarousel(userId: string, userName: string) {
      const userCellEl = document.createElement('div');
      userCellEl.id = userId;
      userCellEl.className = 'user-carousel-cell';
      this.flkty.append(userCellEl);
  }

  removeUserFromOrderCarousel(userId: string) {
    const optUserCellEl = document.getElementById(userId);
    if (!optUserCellEl) throw new Error('No user cell element found');
    this.flkty.remove(optUserCellEl);
  }

}
