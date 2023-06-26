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

  public uploadSong(file: File | UrlExtract): Promise<Response> {
    if (!this.basePath) throw new Error('No base path set');
    // if a file
    if (file instanceof File) {
      const formData = new FormData();
      formData.append('file', file);
      return fetch(this.basePath + `/music/upload/file`, {
              method: 'PUT',
              body: formData
          });
    } else {
      return fetch(this.basePath + `/music/upload/url`, {
              method: 'PUT',
              body: JSON.stringify(file)
          });
    }
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