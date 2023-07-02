export class RestClient {

  private roomId: string;
  private basePath?: string;
  private userId?: string;

  constructor(roomId: string) {
    this.roomId = roomId;
  }

  public setUserId(userId: string) {
    this.userId = userId;
    this.basePath = `/${this.roomId}/users/${this.userId}`;
  }

  public startMusic(): Promise<Response> {
    if (!this.basePath) throw new Error('No base path set');
    return fetch(this.basePath + '/music/start', {
        method: 'PUT'
    });
  }

  public validateUrl(url: string): Promise<string> {
    console.log(JSON.stringify({url}));
    return fetch("/validate-url", {
        method: 'POST',
        headers: {
            'Content-Type': 'application/json'
        },
        body: JSON.stringify({url})
    }).then(res => res.text());
  }

  public uploadSong(song: Song): Promise<Response> {
    if (!this.basePath) throw new Error('No base path set');
    const formData = new FormData();
    if (song instanceof File) {
      formData.append('file', song);
    } else {
      formData.append('url', song.url);
    }
    return fetch(this.basePath + `/music/upload`, {
            method: 'PUT',
            body: formData
        });
  }

  public enqueueSong(req: EnqueueSongRequest): Promise<Response> {
    if (!this.basePath) throw new Error('No base path set');
    return fetch(this.basePath + '/music/start', {
            method: 'PUT',
            body: JSON.stringify(req)
        });
  }

  public removeSong(songId: string): Promise<Response> {
    if (!this.basePath) throw new Error('No base path set');
    return fetch(this.basePath + `/songs/${songId}`, {
            method: 'DELETE'
        });
  }

  
  public reprioritizeSong(songId: string, priority: number): Promise<Response> {
    if (!this.basePath) throw new Error('No base path set');
    return fetch(this.basePath + `/songs/${songId}?priority=${priority}`, {
            method: 'PUT'
        });
  }

  public getHtml(url: string): Promise<string> {
    return fetch(url, {
        method: 'GET',
        headers: {
            'Content-Type': 'text/html'
        }
    }).then(res => res.text());
  }
}