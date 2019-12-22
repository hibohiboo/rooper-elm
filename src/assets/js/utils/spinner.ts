export const hideLoader = () => {

  const activeLoaderClassElement = document.querySelector('.rooper-active');
  if (!activeLoaderClassElement) {
    return;
  }
  activeLoaderClassElement.classList.remove('rooper-active');
};

export const showLoader = () => {
  const activeLoaderClassElement = document.querySelector('body');
  if (!activeLoaderClassElement) {
    return;
  }
  activeLoaderClassElement.classList.add('rooper-active');
};
