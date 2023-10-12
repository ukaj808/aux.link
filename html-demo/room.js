const usersList = document.querySelector('.users');

const staggerUsers = (node, offset, zIndex) => {
    if (node === null) return;
    const calcOffset = 250 / usersList.children.length;
    node.style.left = offset;
    node.style.zIndex = zIndex;
    const nextOffset = offset + calcOffset;
    const nextZIndex = zIndex - 1;
    staggerUsers(node.nextElementSibling, nextOffset, nextZIndex);
};

const calcInitialOffset = () => {
    const offset = 100 / usersList.children.length;
    return offset;
};

const runNextInLineAnimation = () => {
    if (usersList.childElementCount < 2) return;

    const lastPos = { 
        left: usersList.children[usersList.childElementCount-1].style.left,
        zIndex: usersList.children[usersList.childElementCount-1].style.zIndex
    }

    const moveToEndAndSomeAnimation = usersList.children[0].animate([
        {
            left: parseInt(lastPos.left) + 250 + 'px',
        }
    ], 
    {
        duration: 1000,
        fill: 'forwards'
    });
    moveToEndAndSomeAnimation.pause();

    const moveToLastPlaceAnimation = usersList.children[0].animate([
        {
            left: lastPos.left,
        }
    ], 
    {
        duration: 1000,
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
            duration: 1000,
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
            moveToEndAndSomeAnimation.commitStyles();
            moveToEndAndSomeAnimation.cancel();
            const head = usersList.children[0];

            // at this point, the left property should have been updated through the commitStyles() call
            // but we still need to update the zIndex
            for (let i=1; i < usersList.childElementCount; i++) {
                const u = usersList.children[i];
                u.style.zIndex = parseInt(u.style.zIndex) + 1;
            }

            head.remove();
            usersList.appendChild(head);
            head.style.zIndex = 0;

            moveToLastPlaceAnimation.play();
            moveToLastPlaceAnimation.finished.then(() => {
                moveToLastPlaceAnimation.commitStyles();
                moveToLastPlaceAnimation.cancel();
            });
        });
};

const isMobile = window.matchMedia('screen and (min-width:0px) and (max-width:1025px)').matches;

if (isMobile) {
    const offset = calcInitialOffset();
    staggerUsers(usersList.firstElementChild, offset, usersList.children.length - 1);
} else {

}