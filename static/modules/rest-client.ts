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

  public uploadSong(file: File): Promise<Response> {
    if (!this.basePath) throw new Error('No base path set');
    const formData = new FormData();
    formData.append('file', file);
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
}