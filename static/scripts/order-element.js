export class OrderElement {

  #el;
  #orderListEl;

  constructor() {
    this.#el = document.getElementById("order");
    this.#orderListEl = document.getElementById("user-order-list");
  }
  
  addNewUserToOrderList({userId, userName}) {
      const userListItem = document.createElement('li');
      userListItem.id = userId;
      userListItem.class
      userListItem.className = 'full-flex section centered tertiary-theme';

      const uname = document.createElement('span');
      uname.textContent = userName;
      uname.classList.add('user-order-list__username-lbl');

      userListItem.appendChild(uname);

      this.#orderListEl.appendChild(li);
  }

  removeUserFromOrderList(userId) {
    const li = document.getElementById(userId);
    li.remove();
  }

}
