import css, { Declaration, Rule } from 'css';

export class MutableStyleSheet {

    private cssStyleSheet: CSSStyleSheet;

    public constructor(cssStyleSheet: CSSStyleSheet) {
        this.cssStyleSheet = cssStyleSheet;
    }

    public put(id: string, rule: Rule) {
        const index = this.getIndexOfRule(id);
        if (index > -1) {
          this.cssStyleSheet.deleteRule(index);
        }
        this.cssStyleSheet.insertRule(css.stringify(rule));
    }

    public get(id: string): Rule | undefined {
        const stylesheet = css.parse(this.snapshotOfStyleSheet());
        if (stylesheet.stylesheet == null) throw new Error('No stylesheet found');
        const rule = Array.from(stylesheet.stylesheet.rules).find((r) => {
            const rule = r as Rule;
            if (rule.selectors == null) return false;
            return rule.selectors.length === 1 && rule.selectors[0] === '#'+id;
        });
        return rule ? rule as Rule : undefined
    }

    public delete(id: string) {
        const index = this.getIndexOfRule(id);
        if (index > -1) {
          this.cssStyleSheet.deleteRule(index);
        }
    }

    private getIndexOfRule(id: string): number {
        return Array.from(this.cssStyleSheet.cssRules).findIndex((rule) => {
            if (rule instanceof CSSStyleRule) {
                return rule.selectorText === '#'+id;
            }
            return false;
        });
    }

    private snapshotOfStyleSheet(): string {
        return Array.from(this.cssStyleSheet.cssRules).reduce((acc, rule) => { 
            return rule.cssText + acc;
        }, '');
    }

}