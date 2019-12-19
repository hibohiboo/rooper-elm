// import * as M from 'M'; //  tslint-disable-line
import * as Swiper from 'swiper';
import firebaseBackEnd from './firebase/FireBaseBackEnd';
import Room, { addRoom, readRooms } from './firebase/Room';
import { Elm } from './elm/Main'; //  eslint-disable-line import/no-unresolved
import { hideLoader } from './utils/spinner';

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

  // spinnerを消す
  hideLoader();

  // histroy api 設定.遷移 を防ぐ。aタグは増減するので、親でバブリングしたイベントを受ける。
  //  eslint-disable-next-line no-unused-expressions
  document.querySelector('.rooper-container')?.addEventListener('click', (event) => {
    // aタグ以外は処理をしない
    const { target }: { target: any } = event;
    if (target && target.tagName && target.tagName.toUpperCase !== 'A') {
      return;
    }
    // ドメイン外への遷移は制御しない
    const element = target as HTMLAnchorElement;
    if (element.href.indexOf(document.domain) === -1) {
      return;
    }

    event.preventDefault();
    window.history.pushState({}, '惨劇RoopeR online tool', element.href);
    // elm に URLの変更を伝える
    ports.changeUrl.send(element.href);
  });

  // ブラウザの戻るボタンを押したときの挙動を設定
  window.addEventListener('popstate', (event) => { console.log(event); ports.changeUrl.send(window.location.href); });

  // elm に 表示時のページURLを伝える
  ports.changeUrl.send(window.location.href);
};

// 初期設定実行
initApp();
