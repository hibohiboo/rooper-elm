export const hideLoader = () => {
  console.log('hide');
  const activeLoaderClassElement = document.querySelector('.rooper-active');
  if (!activeLoaderClassElement) {
    return;
  }
  activeLoaderClassElement.classList.remove('rooper-active');
};
