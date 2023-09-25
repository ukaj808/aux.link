const usersList = document.querySelector('.users-list');
const offsetMultiplier = 100; //px

const refreshUserPosition = () => {
    const len = usersList.children.length;
    const usersListElements = Array.from(usersList.children);
    for (let i = 0, j = len - 1; i < len; i++,j--) {
        console.log(usersListElements[i]);
        const offset = i * offsetMultiplier;
        const userEl = usersListElements[i];  
        userEl.style.left = offset;
        userEl.style.zIndex = j;
    }
};


const updateView = (event) => {
    console.log('updating view...');
    const first = usersList.firstElementChild;
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