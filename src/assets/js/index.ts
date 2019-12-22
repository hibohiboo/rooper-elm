// import * as M from 'M'; //  tslint-disable-line
import * as Swiper from 'swiper';
import firebaseBackEnd from './firebase/FireBaseBackEnd';
import Room, { addRoom, readRooms } from './firebase/Room';
import { Elm } from './elm/Main'; //  eslint-disable-line import/no-unresolved
import { hideLoader } from './utils/spinner';
import { historyInit, pushHistory } from './utils/history';
import * as Scenario from './firebase/Scenario';

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

  ports.updateScenario.subscribe(scenario => {
    if (!scenario.id) {
      Scenario.addScenario(scenario, firebaseBackEnd.db, firebaseBackEnd.getTimestamp(), user.uid, user.storeUserId);
      pushHistory(ports, `${document.location.protocol}//${document.location.hostname}:${document.location.port}/rooper/scenario`);
    }
  });

  ports.readScenarioNames.subscribe(async () => {
    const scenarioNames = await Scenario.readScenarioNames(firebaseBackEnd.db, user.storeUserId);
    ports.readedScenarioNames.send(scenarioNames);
  });

  ports.readScenario.subscribe(async (scenarioId) => {
    const scenario = await Scenario.readScenario(firebaseBackEnd.db, scenarioId);
    console.log(scenarioId, scenario);
    ports.readedScenario.send(scenario);
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
