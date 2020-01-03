export default class Room {
  public uid: string;

  public createUserId: string;

  public name: string;

  public id: string;

  public script?;

  public createdAt?;

  public updatedAt?;

  constructor({
    createUserId, uid, name, id,
  }) {
    this.createUserId = createUserId;
    this.uid = uid;
    this.name = name;
    this.id = id;
  }
}


/**
 * データベースに部屋を登録する
 *
 * @param json
 * @param db
 * @param timestamp
 * @param uid
 */
export async function addRoom({ name }: Room, db, timestamp, uid, storeUserId) {
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
  const rooms: { id: string, name: string }[] = [];
  await querySnapshot.forEach((doc) => {
    const { id, name } = doc.data();
    rooms.push({ id, name });
  });
  return rooms;
}
