const usersList = document.querySelector('.users-list');

const refreshPositions = (node, offset, zIndex) => {
    if (node === null) return;
    node.style.left = offset;
    node.style.zIndex = zIndex;
    refreshPositions(node.nextElementSibling, offset + 50, zIndex - 1);
};

usersList.addEventListener('click', () => {
    if (usersList.childElementCount < 2) return;

    const lastPos = { 
        left: usersList.children[usersList.childElementCount-1].style.left,
        zIndex: usersList.children[usersList.childElementCount-1].style.zIndex
    }

    const moveToEndAndSomeAnimation = usersList.children[0].animate([
        {
            left: parseInt(lastPos.left) + 250 + 'px',
            zIndex: parseInt(lastPos.zIndex) + 1
        }
    ], 
    {
        duration: 6000,
        fill: 'forwards'
    });
    moveToEndAndSomeAnimation.pause();

    const moveToLastPlaceAnimation = usersList.children[0].animate([
        {
            left: lastPos.left,
            zIndex: lastPos.zIndex
        }
    ], 
    {
        duration: 6000,
        fill: 'forwards'
    });
    moveToLastPlaceAnimation.pause();

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
                left: newPos.left,
            }
        ], 
        {
            duration: 6000,
            fill: 'forwards'
        });
        upOneAnimation.pause();
        oneUpAnimations.push(upOneAnimation);
    }

    // Play all animations
    moveToEndAndSomeAnimation.play();
    oneUpAnimations.forEach(a => a.play());
    Promise.all([moveToEndAndSomeAnimation.finished, ...oneUpAnimations.map(a => a.finished)])
        .then(() => {

            oneUpAnimations.forEach(a => {a.commitStyles(); a.cancel();});

            const head = usersList.children[0];

            moveToEndAndSomeAnimation.commitStyles();
            moveToEndAndSomeAnimation.cancel();

            head.remove();
            usersList.appendChild(head);

            moveToLastPlaceAnimation.play();
            moveToLastPlaceAnimation.finished.then(() => {
                moveToLastPlaceAnimation.commitStyles();
                moveToLastPlaceAnimation.cancel();
            });
        });
});

refreshPositions(usersList.firstElementChild, 0, usersList.children.length);