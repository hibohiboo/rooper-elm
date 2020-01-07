

/**
 * データベースに部屋を登録する
 *
 * @param json
 * @param db
 * @param timestamp
 * @param uid
 */
export async function addRoom({ name }: { name: String }, db, timestamp, uid, storeUserId) {
  const userRef = db.collection('users').doc(storeUserId);
  const ref = await userRef.collection('rooms').doc();
  const { id } = ref;
  const userRoom = {
    name, uid, id, createdAt: timestamp, updatedAt: timestamp,
  };

  const room = {
    id, name, uid, createUserId: storeUserId, createdAt: timestamp, updatedAt: timestamp,
  };

  return Promise.all([
    (await userRef.collection('rooms').doc(id).set(userRoom)), // ユーザ画面に部屋一覧を表示するために名前のみ保持
    (await db.collection('rooms').doc(id).set(room)), // 残りの情報は専用のコレクションに保存
  ]);
}

/**
 * データベースから指定したユーザのルーム一覧を取得する
 *
 * @param db
 * @param storeUserId
 */
export async function readRooms(db, uid, storeUserId) {
  const querySnapshot = await db.collection('users').doc(storeUserId).collection('rooms').where("uid", "==", uid).get();
  const rooms: any[] = [];
  await querySnapshot.forEach((doc) => {
    rooms.push(doc.data());
  });
  return rooms;
}

/**
 * データベースから指定したルーム情報を取得する
 *
 * @param db
 * @param storeUserId
 */
export async function readRoom(db, roomId) {
  const doc = await db.collection('rooms').doc(roomId).get();
  return doc.data();
}

/**
 * ルーム情報を更新する
 * @param obj
 * @param db
 * @param timestamp
 * @param uid
 * @param storeUserId
 */
export async function updateRoom(obj, db, timestamp, uid, storeUserId) {
  const { id, name, scriptId } = obj;
  const roomNameRef = db.collection('users').doc(storeUserId).collection('rooms').doc(id);
  const doc = await roomNameRef.get();
  const roomName = {
    name, scriptId, id, uid, updatedAt: timestamp, createdAt: doc.data().createdAt
  };

  const script = {
    ...obj, id, uid, updatedAt: timestamp, createdAt: doc.data().createdAt
  };

  return Promise.all([
    (await roomNameRef.set(roomName)),
    (await db.collection('rooms').doc(id).set(script)),
  ]);
}
