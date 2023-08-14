document.addEventListener('DOMContentLoaded', (event) => {
    let secondLogo = document.createElement('img');
    secondLogo.src = 'figures/afds_logo.png';
    secondLogo.classList.add('second-logo');
    document.querySelector('.reveal').appendChild(secondLogo);
});
