// import * as M from 'M'; //  tslint-disable-line
import * as Swiper from 'swiper';
import firebaseBackEnd, { hideLoader } from './firebase/FireBaseBackEnd';
import Room, { addRoom } from './firebase/Room';
import { Elm } from './elm/Main'; //  eslint-disable-line import/no-unresolved

require('../css/styles.scss'); // tslint:disable-line no-var-requires


const initApp = async () => {
  const user = await firebaseBackEnd.getSignedInUser();

  // elmのＤＯＭを作成する元となるＤＯＭ要素
  const mountNode: HTMLElement = document.getElementById('app')!;

  // 初期値を与える
  const { ports } = Elm.Main.init({ node: mountNode, flags: user });
  ports.errorToJs.subscribe((data: string) => console.log(data));
  ports.signOut.subscribe(() => firebaseBackEnd.signOut());
  ports.initLoginUI.subscribe(() => firebaseBackEnd.createLoginUi());
  ports.updateRoom.subscribe(({ id, name }) => {
    if (user === null) return;
    const room = new Room({
      createUserId: user.storeUserId, id, name, uid: user.uid,
    });
    if (room.id === '') {
      addRoom(room, firebaseBackEnd.db, firebaseBackEnd.getTimestamp(), user.uid, user.storeUserId);
    }
  });

  let mySwiper;
  ports.initSwiper.subscribe(() => {
    mySwiper = new Swiper('.swiper-container', {
      loop: false,
      // observer: true,
      pagination: {
        el: '.swiper-pagination',
      },
      navigation: {
        nextEl: '.swiper-button-next',
        prevEl: '.swiper-button-prev',
      },
      // ヒストリapi
      history: {
        replaceState: false,
      },
    });
  });

  hideLoader();
};

// 初期設定実行
initApp();
