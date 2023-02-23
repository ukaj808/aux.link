const roomId = location.pathname.substr(1);
const wsProtocol = (location.protocol != null && location.protocol === "https:") ? "wss" : "ws";
const roomsUrl = (location.protocol != null && location.protocol === "https:") ? "augslink-rooms.deno.dev" : "localhost:8080";
const roomMainElement = document.getElementById("room");
let userId;

// Workaround for the fact that js/ts can't serialize/deserialize maps
const reviver = (key, value) => {
    if(typeof value === 'object' && value !== null) {
        if (value.dataType === 'Map') {
            return new Map(value.value);
        }
    }
    return value;
}

const publishRoomEvent = (e) => roomMainElement.dispatchEvent(e);
const createUserWelcomeEvent = (eventData) => new CustomEvent('server-welcome',
    {detail: {userId: eventData.userId, userName: eventData.userName, spotInLine: eventData.spotInLine}});
const createUserEnterEvent = (eventData) => new CustomEvent('user-enter',
    {detail: {userId: eventData.userId, userName: eventData.userName, spotInLine: eventData.spotInLine}});
const createUserLeftEvent = (eventData) => new CustomEvent('user-left',
    {detail: {userId: eventData.userId}});
const createAndPublishServerWelcomeMessage = (eventData) => publishRoomEvent(createUserWelcomeEvent(eventData));
const createAndPublishUserEnterEvent = (eventData) => publishRoomEvent(createUserEnterEvent(eventData));
const createAndPublishUserLeftEvent = (eventData) => publishRoomEvent(createUserLeftEvent(eventData));

const roomMessageHandler = ({data}) => {
    console.log(data);
    let parsedData = JSON.parse(data, reviver);

    switch (parsedData?.type) {
        case "ServerWelcomeMessage":
            createAndPublishServerWelcomeMessage(parsedData);
            break;
        case "UserEnterEvent":
            createAndPublishUserEnterEvent(parsedData);
            break;
        case "UserLeftEvent":
            createAndPublishUserLeftEvent(parsedData);
            break;
        default:
            console.error("Unrecognized Event!");
            break;
    }
}

const createNewUserListElement = (userDetails) => {
    const li = document.createElement('li');
    li.id = userDetails.userId;
    li.classList.add('user-order-list__user');

    const ord = document.createElement('span');
    ord.textContent = userDetails.spotInLine;
    ord.classList.add('user-order-list__order-lbl');

    const uname = document.createElement('span');
    uname.textContent = userDetails.userName;
    uname.classList.add('user-order-list__username-lbl');

    li.appendChild(ord);
    li.appendChild(uname);

    return li;
}

const onUserWelcome = ({detail}) => {
    document.querySelector("#user-order-list").appendChild(createNewUserListElement(detail));
    userId = detail.userId;
}

const onUserJoin = ({detail}) => {
    document.querySelector("#user-order-list").appendChild(createNewUserListElement(detail));
}

const onUserLeft = ({detail}) => {
    const userId = detail.userId;
    document.getElementById(userId).remove();
}

const onLeaveRoom = (userId) => {
    document.getElementById(userId).remove();
}


const roomElement = document.getElementById("room");

roomElement.addEventListener("server-welcome", onUserWelcome);
roomElement.addEventListener("user-enter", onUserJoin);
roomElement.addEventListener("user-left", onUserLeft);

const connect = () => {
    let ws;
    if (ws) ws.close();
    ws = new WebSocket(`${wsProtocol}://${roomsUrl}/${roomId}/${wsProtocol}`);
    ws.addEventListener("message", roomMessageHandler);
    window.addEventListener("beforeunload", () => onLeaveRoom(userId));
};

// Connect to web socket AFTER all the room modules have loaded (e.g. Order Section)
// Each module adds event listeners to the room page (main)...
// If the websocket connection happens before the event listeners are added in each module
// then those modules won't know that they/or another user joined...
// Still noticing this bug in prod!!
window.addEventListener('load', connect)
