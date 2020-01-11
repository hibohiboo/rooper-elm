// import * as M from 'M'; //  tslint-disable-line
import * as Swiper from 'swiper';
import firebaseBackEnd from './firebase/FireBaseBackEnd';
import { Elm } from './elm/Main'; //  eslint-disable-line import/no-unresolved
import { hideLoader, showLoader } from './utils/spinner';
import { historyInit, pushHistory } from './utils/history';
import { commonPorts, logginedPorts } from './ports';
require('../css/styles.scss'); // tslint:disable-line no-var-requires


const initApp = async () => {
  const user = await firebaseBackEnd.getSignedInUser();

  const mountNode: HTMLElement = document.getElementById('app')!;
  const { ports } = Elm.Main.init({ node: mountNode, flags: user });
  commonPorts(ports, firebaseBackEnd);
  if (user === null) {
    historyInit(ports);
    hideLoader();
    return;
  }
  logginedPorts(ports, firebaseBackEnd, user);

  // let mySwiper;
  // ports.initSwiper.subscribe(() => {
  //   mySwiper = new Swiper('.swiper-container', {
  //     loop: false,
  //     // observer: true,
  //     pagination: {
  //       el: '.swiper-pagination',
  //     },
  //     navigation: {
  //       nextEl: '.swiper-button-next',
  //       prevEl: '.swiper-button-prev',
  //     },
  //     // ヒストリapi
  //     history: {
  //       replaceState: false,
  //     },
  //   });
  // });
  historyInit(ports);

  // spinnerを消す
  hideLoader();
};

// 初期設定実行
initApp();
