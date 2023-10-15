const usersList = document.querySelector('.users');

const staggerUsers = (node, zIndex) => {
    if (node === null) return;
    node.style.marginLeft = -100;
    node.style.zIndex = zIndex;
    const nextZIndex = zIndex - 1;
    staggerUsers(node.nextElementSibling, nextZIndex);
};

const runNextInLineAnimation = () => {
    if (usersList.childElementCount < 2) return;

    const lastPos = { 
        marginLeft: usersList.children[usersList.childElementCount-1].style.marginLeft,
        zIndex: usersList.children[usersList.childElementCount-1].style.zIndex
    }

    const moveToEndAndSomeAnimation = usersList.children[0].animate([
        {
            marginLeft: parseInt(lastPos.marginLeft) + 250 + 'px',
        }
    ], 
    {
        duration: 1000,
        fill: 'forwards'
    });
    moveToEndAndSomeAnimation.pause();

    const moveToLastPlaceAnimation = usersList.children[0].animate([
        {
            marginLeft: lastPos.marginLeft,
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
            marginLeft: u2.style.marginLeft,
            zIndex: u2.style.zIndex
        };
        const upOneAnimation = u1.animate([
            {
                marginLeft: newPos.marginLeft,
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
    staggerUsers(usersList.firstElementChild.nextElementSibling, usersList.children.length - 1);
} else {

}