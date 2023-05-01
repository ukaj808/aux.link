/**
 * Worker message event handler.
 * This will initialize worker with FreeQueue instance and set loop for audio
 * processing. 
 */

let ws;
let sharedBuffer;
let roomId;
let userId;

const onWsMessage = (event) => {
  console.log(event);
};

self.onmessage = (msg) => {
  console.log(msg);
  if (msg.data.type === "init") {
    roomId = msg.data.roomId;
    userId = msg.data.userId;
    ws = new WebSocket(`ws://localhost:8080/${roomId}/${userId}/audio/listen`);
    ws.binaryType = 'arraybuffer';
    sharedBuffer = msg.data.sharedBuffer;
    ws.addEventListener("message", onWsMessage); 
  } else if (msg.data.type === 'destroy') {
    ws.removeEventListener(onWsMessage);
    ws.close();
    ws = null;
    sharedBuffer = null;
  }
};
