const usersList = document.querySelector('.users-list');

const setInitialPositions = (node, offset, zIndex) => {
    if (node === null) return;
    node.style.left = offset;
    node.style.zIndex = zIndex;
    setInitialPositions(node.nextElementSibling, offset + 50, zIndex - 1);
};

const shiftUserElements = (head) => {
    if (head === null) return;
    const go = (node) => {
        if (node === null) return;
        if (node.nextElementSibling === null) {
            const headPos = { left: head.style.left, zIndex: head.style.zIndex };
            head.style.left = node.style.left;
            head.style.zIndex = node.style.zIndex;
            node.style.left = node.previousElementSibling.style.left;
            node.style.zIndex = node.previousElementSibling.style.zIndex;
            return headPos;
        }
       const headPos = go(node.nextElementSibling);
       node.style.left = node.previousElementSibling.style.left;
       node.style.zIndex = node.previousElementSibling.style.zIndex;
       return headPos;
    };
    const headPos = go(head.nextElementSibling);
    head.nextElementSibling.style.left = headPos.left;
    head.nextElementSibling.style.zIndex = headPos.zIndex;
}

const updateView = (event) => {
    const transition = document.startViewTransition(() => shiftUserElements(usersList.firstElementChild));
    transition.finished.then(() => {
    });
}

setInitialPositions(usersList.firstElementChild, 0, usersList.children.length);
usersList.addEventListener('click', updateView);