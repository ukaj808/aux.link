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

  addThisUserToOrderCarousel(userId: string, userName: string) {
    this.addNewUserToOrderCarousel(userId, userName);
  }
  
  addNewUserToOrderCarousel(userId: string, userName: string, isCreator: boolean = false) {
      const userCellEl = document.createElement('div');
      userCellEl.id = userId;
      userCellEl.className = 'user-carousel-cell';
      if (isCreator) this.addCreatorOverlay(userCellEl);
      this.flkty.append(userCellEl);
  }

  removeUserFromOrderCarousel(userId: string) {
    const optUserCellEl = document.getElementById(userId);
    if (!optUserCellEl) throw new Error('No user cell element found');
    this.flkty.remove(optUserCellEl);
  }

  private addCreatorOverlay(userCell: HTMLDivElement) {

  }

  private removeCreatorOverlay(userCell: HTMLDivElement) {

  }

}
