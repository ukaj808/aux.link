import { RestClient } from "./rest-client";


export class UserElement {
  private el: HTMLElement;
  private userId: string;
  private isCreator: boolean;
  private restClient: RestClient;
  private serverGenerated: boolean;

  constructor(restClient: RestClient, userId: string, isCreator: boolean, serverGenerated: boolean) {
    this.userId = userId;
    this.isCreator = isCreator;
    this.restClient = restClient;
    this.serverGenerated = serverGenerated;

    if (serverGenerated) {
      const userEl = document.getElementById(userId);
      if (!userEl) throw new Error('No user element found');
      this.el = userEl;
    } else {
      const userEl = document.createElement('div');
      userEl.id = userId;
      userEl.className = 'user-carousel-cell';
      if (isCreator) this.addCreatorOverlay(userEl);
      this.el = userEl;
    }
  }

  public getEl(): HTMLElement {
    return this.el;
  }

  public getUserId(): string {
    return this.userId;
  }

  private addCreatorOverlay(userCellEl: HTMLElement) {
    userCellEl.classList.add('overlay-sect', 'overlay');

    const overlayEl = document.createElement('div');
    overlayEl.classList.add('overlay', 'full-flex', 'centered');

    overlayEl.onclick = () => {
      this.restClient.startMusic().then(() => {
        userCellEl.classList.toggle('overlay');
      }).catch((err) => {
        console.error("Error calling the start music api", err);
      });
    };

    userCellEl.appendChild(overlayEl);
  }
}

export class UserElementFactory {
  private userElements: UserElement[];
  private restClient: RestClient;

  constructor(restClient: RestClient, userCarouselEl: Element) {
    this.userElements = [];
    this.restClient = restClient;
    Array.from(userCarouselEl.children).forEach((user) => {
      this.userElements.push(new UserElement(restClient, user.id, false, true));
    });
  }

  public createNewUser(userId: string, username: string, isCreator: boolean): UserElement {
    const newUser = new UserElement(this.restClient, userId, isCreator, false);
    this.userElements.push(newUser);
    return newUser;
  }

  public getUser(userId: string): UserElement {
    const optUser = this.userElements.find((user) => user.getUserId() === userId);
    if (!optUser) throw new Error('No user found');
    return optUser;
  }

  public removeUser(userId: string) {
    const optUser = this.userElements.find((user) => user.getUserId() === userId);
    if (!optUser) throw new Error('No user found');
    this.userElements = this.userElements.filter((user) => user.getUserId() !== userId);
  }

}

