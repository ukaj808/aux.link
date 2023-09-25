const usersList = document.querySelector('.users-list');
const offsetMultiplier = 100; // px

const refreshUserPosition = () => {
    const len = usersList.children.length;
    const usersListElements = Array.from(usersList.children);
    for (let i = 0, j = len - 1; i < len; i++,j--) {
        const offset = i * offsetMultiplier;
        const userEl = usersListElements[i];  
        userEl.style.left = offset;
        userEl.style.zIndex = j;
    }
};


const updateView = (event) => {
    console.log('updating view...');
    const first = usersList.firstElementChild;
    // This seems to be using a smooth-cross fade effect since were removing and adding the elements back to the dom?
    // Maybe I can try just altering the styles properties instead...
    const sendFirstUserToBackOfList = () => {
        const user1 = usersList.firstElementChild;
        usersList.firstElementChild.remove();
        usersList.appendChild(user1);
        refreshUserPosition();
    }

    const transition = document.startViewTransition(() => sendFirstUserToBackOfList());
}
refreshUserPosition();
usersList.addEventListener('click', updateView);