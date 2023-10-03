const usersList = document.querySelector('.users-list');

const setInitialPositions = (node, offset, zIndex) => {
    if (node === null) return;
    node.style.left = offset;
    node.style.zIndex = zIndex;
    setInitialPositions(node.nextElementSibling, offset + 50, zIndex - 1);
};

usersList.addEventListener('click', () => {
    if (usersList.childElementCount < 2) return;

    const lastPos = { 
        left: usersList.children[usersList.childElementCount-1].style.left,
        zIndex: usersList.children[usersList.childElementCount-1].style.zIndex
    }

    const firstToLastAnimation = usersList.children[0].animate([
        {
            left: parseInt(lastPos.left) + 250 + 'px',
            offset: 0.5
        },
        {
            left: lastPos.left,
            zIndex: lastPos.zIndex
        }
    ], 
    {
        duration: 2000,
        fill: 'forwards'
    });
    firstToLastAnimation.pause();
    const oneUpAnimations = [];
    for (let i = 1; i < usersList.childElementCount; i++) {
        const u1 = usersList.children[i];
        const u2 = usersList.children[i-1]; // User ahead one position
        const newPos = {
            left: u2.style.left,
            zIndex: u2.style.zIndex
        };
        const upOneAnimation = u1.animate([
            {
                offset: 0.5,
                left: ((parseInt(newPos.left) + parseInt(u1.style.left)) / 2) + 'px',
            },
            {
                left: newPos.left,
                zIndex: newPos.zIndex
            }
        ], 
        {
            duration: 2000,
            fill: 'forwards'
        });
        upOneAnimation.pause();
        oneUpAnimations.push(upOneAnimation);
    }

    // Play all animations
    firstToLastAnimation.play();
    oneUpAnimations.forEach(a => a.play());
    Promise.all([firstToLastAnimation.finished, ...oneUpAnimations.map(a => a.finished)])
        .then(() => {
            firstToLastAnimation.commitStyles();
            oneUpAnimations.forEach(a => a.commitStyles());
            firstToLastAnimation.cancel();
            oneUpAnimations.forEach(a => a.cancel());
            const head = usersList.children[0];
            head.remove();
            usersList.appendChild(head);
        });
});


setInitialPositions(usersList.firstElementChild, 0, usersList.children.length);