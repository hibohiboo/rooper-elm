// import * as M from 'M'; //  tslint-disable-line
import * as Swiper from 'swiper';
import firebaseBackEnd, { hideLoader } from './firebase/FireBaseBackEnd';
import Room, { addRoom, readRooms } from './firebase/Room';
import { Elm } from './elm/Main'; //  eslint-disable-line import/no-unresolved

require('../css/styles.scss'); // tslint:disable-line no-var-requires


const initApp = async () => {
  const user = await firebaseBackEnd.getSignedInUser();

  const mountNode: HTMLElement = document.getElementById('app')!;
  const { ports } = Elm.Main.init({ node: mountNode, flags: user });
  ports.errorToJs.subscribe((data: string) => console.log(data));
  ports.signOut.subscribe(() => firebaseBackEnd.signOut());
  ports.initLoginUI.subscribe(() => firebaseBackEnd.createLoginUi());
  if (user === null) {
    return;
  }
  let rooms = await readRooms(firebaseBackEnd.db, user.storeUserId);
  if (rooms.length === 0) {
    const room = new Room({
      createUserId: user.storeUserId, id: '', name: `room-${user.twitterScreenName}`, uid: user.uid,
    });
    addRoom(room, firebaseBackEnd.db, firebaseBackEnd.getTimestamp(), user.uid, user.storeUserId);

    // 再取得
    rooms = await readRooms(firebaseBackEnd.db, user.storeUserId);
  }
  ports.readRooms.send(rooms);
  // ports.updateRoom.subscribe(({ id, name }) => {
  //   const room = new Room({
  //     createUserId: user.storeUserId, id, name, uid: user.uid,
  //   });
  //   if (room.id === '') {
  //     addRoom(room, firebaseBackEnd.db, firebaseBackEnd.getTimestamp(), user.uid, user.storeUserId);
  //   }
  // });

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

  hideLoader();

  // histroy api 設定.遷移 を防ぐ
  [...document.querySelectorAll('a')].forEach((element) => element.addEventListener('click', (event) => {
    // console.log('href', element.href);
    // console.log('domain', document.domain);
    if (element.href.indexOf(document.domain) !== -1) {
      // ページ遷移キャンセル
      event.preventDefault();
      // event.stopPropagation(); // 親にイベントを伝播させない
      // event.stopImmediatePropagation(); // 他のリスナを実行しない

      // elm に URLの変更を伝える
      ports.changeUrl.send(element.href);
    }
    // Cancel the event as stated by the standard.
    // Chrome requires returnValue to be set.
    event.returnValue = false; // eslint-disable-line
    return false;
  }));
  // elm に ページ遷移時の URLを伝える
  ports.changeUrl.send(window.location.href);
};

// 初期設定実行
initApp();
