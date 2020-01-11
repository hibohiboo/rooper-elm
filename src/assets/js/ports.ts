import { historyInit, pushHistory } from './utils/history';
import * as Room from './firebase/Room';
import * as Script from './firebase/Script';
import { hideLoader, showLoader } from './utils/spinner';

export function commonPorts(ports, firebaseBackEnd) {
  ports.errorToJs.subscribe((data: string) => console.log(data));
  ports.signOut.subscribe(() => firebaseBackEnd.signOut());
  ports.initLoginUI.subscribe(() => {
    if (window.location.href.indexOf('room') !== -1 && window.location.href.indexOf('edit') === -1) { return; }// 部屋の見学の時にはログインさせない
    firebaseBackEnd.createLoginUi()
  });
}

export async function logginedPorts(ports, firebaseBackEnd, user) {
  let rooms = await Room.readRooms(firebaseBackEnd.db, user.uid, user.storeUserId);
  if (rooms.length === 0) {
    const room = {
      createUserId: user.storeUserId, id: '', name: `room-${user.twitterScreenName}`, uid: user.uid,
    };
    Room.addRoom(room, firebaseBackEnd.db, firebaseBackEnd.getTimestamp(), user.uid, user.storeUserId);

    // 再取得
    rooms = await Room.readRooms(firebaseBackEnd.db, user.uid, user.storeUserId);
  }
  ports.readedRooms.send(rooms);
  ports.readRooms.subscribe(async () => {
    const readRooms = await Room.readRooms(firebaseBackEnd.db, user.uid, user.storeUserId);
    ports.readedRooms.send(readRooms);
  });


  ports.readRoom.subscribe(async (roomId) => {
    showLoader();
    const room = await Room.readRoom(firebaseBackEnd.db, roomId);
    ports.readedRoom.send(room);
    hideLoader();
  });


  ports.updateScript.subscribe(async script => {
    if (!script.id) {
      await Script.addScript(script, firebaseBackEnd.db, firebaseBackEnd.getTimestamp(), user.uid, user.storeUserId);
    } else {
      await Script.updateScript(script, firebaseBackEnd.db, firebaseBackEnd.getTimestamp(), user.uid, user.storeUserId);
    }

    const port = document.location.port ? `:${document.location.port}` : '';
    // /script/のように、最後の/がないとfirebaseでエラーとなる
    pushHistory(ports, `${document.location.protocol}//${document.location.hostname}${port}/rooper/script/`);
  });

  ports.readScriptNames.subscribe(async () => {
    const scriptNames = await Script.readScriptNames(firebaseBackEnd.db, user.storeUserId);
    ports.readedScriptNames.send(scriptNames);
  });

  ports.readScript.subscribe(async (scriptId) => {
    showLoader();
    const script = await Script.readScript(firebaseBackEnd.db, user.uid, scriptId);
    console.log(scriptId, script);
    ports.readedScript.send(script);
    hideLoader();
  });
  ports.readScriptForRoom.subscribe(async (scriptId) => {
    showLoader();
    const script = await Script.readScript(firebaseBackEnd.db, user.uid, scriptId);
    ports.readedScriptForRoom.send(script);
    hideLoader();
  });

  ports.deleteScript.subscribe(async (scriptId) => {
    showLoader();
    const result = await Script.deleteScript(firebaseBackEnd.db, user.storeUserId, scriptId);
    ports.deletedScript.send(result);
    hideLoader();
  });
  ports.updateRoom.subscribe(async (room: any) => {
    if (!room.id) {
      await Room.addRoom(room, firebaseBackEnd.db, firebaseBackEnd.getTimestamp(), user.uid, user.storeUserId);
    } else {
      await Room.updateRoom(room, firebaseBackEnd.db, firebaseBackEnd.getTimestamp(), user.uid, user.storeUserId);
    }
    const port = document.location.port ? `:${document.location.port}` : '';
    // /script/のように、最後の/がないとfirebaseでエラーとなる
    pushHistory(ports, `${document.location.protocol}//${document.location.hostname}${port}/rooper/`);
  });
}
