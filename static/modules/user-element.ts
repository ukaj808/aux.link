import { RestClient } from "./rest-client";
import { SvgFactory } from "./svg";


export class UserElement {
  private el: HTMLLIElement;
  private userId: string;
  private isCreator: boolean;
  private restClient: RestClient;
  private svgFactory: SvgFactory;
  private serverGenerated: boolean;

  constructor(restClient: RestClient, svgFactory: SvgFactory, userId: string, isCreator: boolean, serverGenerated: boolean) {
    this.userId = userId;
    this.isCreator = isCreator;
    this.restClient = restClient;
    this.svgFactory = svgFactory;
    this.serverGenerated = serverGenerated;

    if (serverGenerated) {
      const userEl = document.getElementById(userId) as HTMLLIElement;
      if (!userEl) throw new Error('No user element found');
      this.el = userEl;
    } else {
      const userEl = document.createElement('li');
      userEl.id = userId;
      userEl.className = 'square-cell';
      if (isCreator) this.addCreatorOverlay(userEl);
      this.el = userEl;
    }
  }

  public getEl(): HTMLLIElement {
    return this.el;
  }

  public getUserId(): string {
    return this.userId;
  }

  private addCreatorOverlay(userCellEl: HTMLElement) {
    userCellEl.classList.add('pos-rel', 'full-flex', 'centered');

    const overlayEl = document.createElement('div');
    overlayEl.classList.add('overlay', 'full-flex', 'centered');

    overlayEl.appendChild(this.svgFactory.generatePlayIcon());

    overlayEl.onclick = () => {
      this.restClient.startMusic().then(() => {
        overlayEl.classList.add('invisible');
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
  private svgFactory: SvgFactory;

  constructor(restClient: RestClient, svgFactory: SvgFactory, userLi: HTMLOListElement) {
    this.userElements = [];
    this.svgFactory = svgFactory;
    this.restClient = restClient;
    Array.from(userLi.children).forEach((user) => {
      this.userElements.push(new UserElement(restClient, svgFactory, user.id, false, true));
    });
  }

  public createNewUser(userId: string, username: string, isCreator: boolean): UserElement {
    const newUser = new UserElement(this.restClient, this.svgFactory, userId, isCreator, false);
    this.userElements.push(newUser);
    return newUser;
  }

  public getUser(userId: string): UserElement {
    const optUser = this.userElements.find((user) => user.getUserId() === userId);
    if (!optUser) throw new Error('No user found');
    return optUser;
  }

  public removeUser(userId: string) {
    const optUser = this.userElements.find((user) => { 
      return user.getUserId() === userId 
    });
    if (!optUser) throw new Error('No user found');
    this.userElements = this.userElements.filter((user) => user.getUserId() !== userId);
  }

}

