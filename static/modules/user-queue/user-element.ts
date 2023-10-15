import { RestClient } from "../rest-client";
import { SvgFactory } from "../svg";


export class UserElement {
  private el: HTMLDivElement;
  private userId: string;
  private hexColor: string;
  private isCreator: boolean;
  private restClient: RestClient;
  private svgFactory: SvgFactory;
  private serverGenerated: boolean;

  constructor(restClient: RestClient, svgFactory: SvgFactory, userId: string, hexColor: string, isCreator: boolean, serverGenerated: boolean) {
    this.userId = userId;
    this.hexColor = hexColor;
    this.isCreator = isCreator;
    this.restClient = restClient;
    this.svgFactory = svgFactory;
    this.serverGenerated = serverGenerated;

    if (serverGenerated) {
      const userEl = document.getElementById(userId) as HTMLDivElement;
      if (!userEl) throw new Error('No user element found');
      this.el = userEl;
    } else {
      const userEl = document.createElement('div');
      userEl.id = userId;
      userEl.classList.add('square-cell', 'tertiary-theme', 'spaced-hz-li');
      userEl.style.backgroundColor = hexColor;
      if (isCreator) this.addCreatorOverlay(userEl);
      this.el = userEl;
    }
  }

  public getEl(): HTMLDivElement {
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
  private userElements: UserElement[]; //todo: Remove?
  private restClient: RestClient;
  private svgFactory: SvgFactory;

  constructor(restClient: RestClient, svgFactory: SvgFactory, userSectionEl: HTMLElement) {
    this.userElements = [];
    this.svgFactory = svgFactory;
    this.restClient = restClient;
    Array.from(userSectionEl.children).forEach((user) => {
      const hexColor = user.getAttribute('data-hex-color');
      if (!hexColor) throw new Error('No hex color found');
      this.userElements.push(new UserElement(restClient, svgFactory, user.id, hexColor, false, true));
    });
  }

  public createNewUser(userId: string, username: string, hexColor: string, isCreator: boolean): UserElement {
    const newUser = new UserElement(this.restClient, this.svgFactory, userId, hexColor, isCreator, false);
    this.userElements.push(newUser);
    return newUser;
  }

  public getUser(userId: string): UserElement {
    const optUser = this.userElements.find((user) => user.getUserId() === userId);
    if (!optUser) throw new Error('No user found');
    return optUser;
  }

}

