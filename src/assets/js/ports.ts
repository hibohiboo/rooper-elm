import { historyInit, pushHistory } from './utils/history';
import * as Room from './firebase/Room';
import * as RoomData from './firebase/RoomData';
import * as Script from './firebase/Script';
import { hideLoader, showLoader } from './utils/spinner';
import { userInfo } from 'os';
import { createZip } from './udonarium';

export function commonPorts(ports, firebaseBackEnd, user) {
  ports.errorToJs.subscribe((data: string) => console.log(data));
  ports.signOut.subscribe(() => firebaseBackEnd.signOut());
  ports.initLoginUI.subscribe(() => {
    if (
      window.location.href.indexOf('room') !== -1 &&
      window.location.href.indexOf('edit') === -1
    ) {
      return;
    } // 部屋の見学の時にはログインさせない
    firebaseBackEnd.createLoginUi();
  });
  ports.listenRoomData.subscribe(async roomId => {
    if (!user) {
      RoomData.listenRoomData(firebaseBackEnd.db, roomId, ports);
      return;
    }
    showLoader();
    const room = await Room.readRoom(firebaseBackEnd.db, roomId);
    if (room.uid === user.uid) {
      ports.readedRoomForRoomData.send(room);
    }
    RoomData.listenRoomData(firebaseBackEnd.db, roomId, ports);
    hideLoader();
  });
}

export async function logginedPorts(ports, firebaseBackEnd, user) {
  // 初期設定
  let myRooms = await Room.readRooms(
    firebaseBackEnd.db,
    user.uid,
    user.storeUserId
  );

  if (myRooms.length === 0) {
    const room = {
      createUserId: user.storeUserId,
      id: '',
      name: `room-${user.twitterScreenName}`,
      uid: user.uid
    };
    Room.addRoom(
      room,
      firebaseBackEnd.db,
      firebaseBackEnd.getTimestamp(),
      user.uid,
      user.storeUserId
    );

    // 再取得
    myRooms = await Room.readRooms(
      firebaseBackEnd.db,
      user.uid,
      user.storeUserId
    );
  }
  ports.readedMyRooms.send(myRooms);
  ports.readedRooms.send(
    await Room.readRoomNames(
      firebaseBackEnd.db,
      user.uid,
      user.twitterScreenName
    )
  );

  // 取得イベント時
  ports.readRooms.subscribe(async () => {
    ports.readedMyRooms.send(
      await Room.readRooms(firebaseBackEnd.db, user.uid, user.storeUserId)
    );
    ports.readedRooms.send(
      await Room.readRoomNames(
        firebaseBackEnd.db,
        user.uid,
        user.twitterScreenName
      )
    );
  });

  ports.readRoom.subscribe(async roomId => {
    showLoader();
    const room = await Room.readRoom(firebaseBackEnd.db, roomId);
    ports.readedRoom.send(room);
    hideLoader();
  });

  ports.updateScript.subscribe(async script => {
    if (!script.id) {
      await Script.addScript(
        script,
        firebaseBackEnd.db,
        firebaseBackEnd.getTimestamp(),
        user.uid,
        user.storeUserId
      );
    } else {
      await Script.updateScript(
        script,
        firebaseBackEnd.db,
        firebaseBackEnd.getTimestamp(),
        user.uid,
        user.storeUserId
      );
    }

    const port = document.location.port ? `:${document.location.port}` : '';
    // /script/のように、最後の/がないとfirebaseでエラーとなる
    pushHistory(
      ports,
      `${document.location.protocol}//${document.location.hostname}${port}/rooper/script/`
    );
  });

  ports.readScriptNames.subscribe(async () => {
    const scriptNames = await Script.readScriptNames(
      firebaseBackEnd.db,
      user.storeUserId
    );
    ports.readedScriptNames.send(scriptNames);
  });

  ports.readScript.subscribe(async scriptId => {
    showLoader();
    const script = await Script.readScript(
      firebaseBackEnd.db,
      user.uid,
      scriptId
    );
    console.log(scriptId, script);
    ports.readedScript.send(script);
    hideLoader();
  });
  ports.readScriptForRoom.subscribe(async scriptId => {
    showLoader();
    const script = await Script.readScript(
      firebaseBackEnd.db,
      user.uid,
      scriptId
    );
    ports.readedScriptForRoom.send(script);
    hideLoader();
  });

  ports.deleteScript.subscribe(async scriptId => {
    showLoader();
    const result = await Script.deleteScript(
      firebaseBackEnd.db,
      user.storeUserId,
      scriptId
    );
    ports.deletedScript.send(result);
    hideLoader();
  });
  ports.updateRoom.subscribe(async (room: any) => {
    if (!room.id) {
      await Room.addRoom(
        room,
        firebaseBackEnd.db,
        firebaseBackEnd.getTimestamp(),
        user.uid,
        user.storeUserId
      );
    } else {
      await Room.updateRoom(
        room,
        firebaseBackEnd.db,
        firebaseBackEnd.getTimestamp(),
        user.uid,
        user.storeUserId
      );
    }
    const port = document.location.port ? `:${document.location.port}` : '';
    // /script/のように、最後の/がないとfirebaseでエラーとなる
    pushHistory(
      ports,
      `${document.location.protocol}//${document.location.hostname}${port}/rooper/`
    );
  });

  ports.updateRoomData.subscribe(async data => {
    showLoader();
    await RoomData.updateRoomData(
      data,
      firebaseBackEnd.db,
      firebaseBackEnd.getTimestamp(),
      user.uid
    );
    hideLoader();
  });

  ports.exportUdonariumData.subscribe(async script => {
    console.log(script);
    createZip(script.characters);
  });
}
