/**
 * Worker message event handler.
 * This will initialize worker with FreeQueue instance and set loop for audio
 * processing. 
 */

let ws;
let sharedBuffer;

const onWsMessage = (event) => {
  console.log(event);
};

self.onmessage = (msg) => {
  console.log(msg);
  if (msg.data.type === "init") {
    ws = new WebSocket(`ws://${roomsUrl}/${roomId}/${userId}/audio/listen`);
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
