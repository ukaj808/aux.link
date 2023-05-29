export class OrderElement {

  #orderCarouselEl;
  #flkty;

  constructor() {
    this.#orderCarouselEl = document.querySelector(".user-carousel");
    this.#flkty = new Flickity( this.#orderCarouselEl, {
      // options
      cellAlign: 'center',
      contain: true
    });
  }
  
  addNewUserToOrderCarousel(userId, userName) {
      const userCellEl = document.createElement('div');
      userCellEl.id = userId;
      userCellEl.className = 'user-carousel-cell';
      this.#flkty.append(userCellEl);
  }

  removeUserFromOrderCarousel(userId) {
    const userCellEl = document.getElementById(userId);
    this.#flkty.remove(userCellEl);
  }

}
