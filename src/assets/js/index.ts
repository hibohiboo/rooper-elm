// import * as M from 'M'; //  tslint-disable-line
import * as Swiper from 'swiper';
import firebaseBackEnd from './firebase/FireBaseBackEnd';
import Room, { addRoom, readRooms } from './firebase/Room';
import { Elm } from './elm/Main'; //  eslint-disable-line import/no-unresolved
import { hideLoader, showLoader } from './utils/spinner';
import { historyInit, pushHistory } from './utils/history';
import * as Script from './firebase/Script';

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
  ports.readedRooms.send(rooms);

  ports.updateScript.subscribe(async script => {
    if (!script.id) {
      await Script.addScript(script, firebaseBackEnd.db, firebaseBackEnd.getTimestamp(), user.uid, user.storeUserId);
    } else {
      await Script.updateScript(script, firebaseBackEnd.db, firebaseBackEnd.getTimestamp(), user.uid, user.storeUserId);
    }

    // /script/のように、最後の/がないとfirebaseでエラーとなる
    pushHistory(ports, `${document.location.protocol}//${document.location.hostname}:${document.location.port}/rooper/script/`);
  });

  ports.readScriptNames.subscribe(async () => {
    const scriptNames = await Script.readScriptNames(firebaseBackEnd.db, user.storeUserId);
    ports.readedScriptNames.send(scriptNames);
  });

  ports.readScript.subscribe(async (scriptId) => {
    showLoader();
    const script = await Script.readScript(firebaseBackEnd.db, scriptId);
    console.log(scriptId, script);
    ports.readedScript.send(script);
    hideLoader();
  });

  ports.deleteScript.subscribe(async (scriptId) => {
    showLoader();
    const result = await Script.deleteScript(firebaseBackEnd.db, user.storeUserId, scriptId);
    ports.deletedScript.send(result);
    hideLoader();
  });
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
  historyInit(ports);

  // spinnerを消す
  hideLoader();
};

// 初期設定実行
initApp();
