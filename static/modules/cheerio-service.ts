export class HtmlParser {
    public getTitle(html: string): string {
        const titleStartIndex = html.indexOf('<title>');
        const titleEndIndex = html.indexOf('</title>');
        if (titleStartIndex !== -1 && titleEndIndex !== -1 && titleEndIndex > titleStartIndex) {
            const pageTitle = html.substring(titleStartIndex + 7, titleEndIndex);
            return pageTitle;
        } else {
            throw new Error('Title tag not found in the HTML');
        }
    }
}