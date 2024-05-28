document.addEventListener('DOMContentLoaded', () => {
  const teamMembers = document.querySelectorAll('.team-member');
  const overlay = document.getElementById('overlay');
  const overlayImg = document.getElementById('overlay-img');
  const overlayInfo = document.getElementById('overlay-info');
  const closeOverlay = document.querySelector('.close-overlay');

  teamMembers.forEach(member => {
    member.addEventListener('click', () => {
      const info = member.getAttribute('data-info');
      const img = member.getAttribute('data-img');

      overlayImg.src = img;
      overlayInfo.textContent = info;
      
      overlay.style.display = 'flex';
    });
  });

  closeOverlay.addEventListener('click', () => {
    overlay.style.display = 'none';
  });

  overlay.addEventListener('click', (event) => {
    if (event.target === overlay) {
      overlay.style.display = 'none';
    }
  });
});
