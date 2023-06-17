export class RoomMessagePublisher {

  // I might be over engineering here... My thought is that the server should wait until 
  // all clients have prepped there audio context before flipping on the stream 
  // I dont think this is necesarry though since the audio context prep is synchronous function and will
  // always work. The AudioPrepCommand is asynchronous though and every user is not guaranteed to receive it/
  // receive it at the same time. Question: do i know server side if the command was received by the client? Is that
  // ws event publishing a synchronous action on the server side?
  constructor() {
  }

  public publish = (message: UserMessage) => {
  }
}
