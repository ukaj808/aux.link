export class UserMessageProcessor {

  #orderElement;

  constructor(orderElement) {
    this.#orderElement = orderElement;
  }

  process(data) {
    console.log(data);

    switch (data?.type) {
        case "ServerWelcomeMessage":
            this.#orderElement.addNewUserToOrderList(data.userId, data.userName);
            break;
        case "UserEnterEvent":
            this.#orderElement.addNewUserToOrderList(data.userId, data.userName);
            break;
        case "UserLeftEvent":
            this.#orderElement.removeUserFromOrderList(data.userId, data.userName);
            break;
        default:
            console.error("Unrecognized Event!");
            break;
    }
  }
}
