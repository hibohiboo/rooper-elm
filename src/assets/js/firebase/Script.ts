/**
 * データベースにシナリオを登録する
 *
 * @param json
 * @param db
 * @param timestamp
 * @param uid
 */
export async function addScript(obj, db, timestamp, uid, storeUserId) {
  const userRef = db.collection('users').doc(storeUserId);
  const ref = await userRef.collection('scripts').doc();
  const { id } = ref;
  const userScript = {
    name: obj.name, uid, id, createdAt: timestamp, updatedAt: timestamp,
  };

  const script = {
    ...obj, id, uid, createUserId: storeUserId, createdAt: timestamp, updatedAt: timestamp,
  };

  return Promise.all([
    (await userRef.collection('scripts').doc(id).set(userScript)), // ユーザ画面に部屋一覧を表示するために名前のみ保持
    (await db.collection('scripts').doc(id).set(script)), // 残りの情報は専用のコレクションに保存
  ]);
}

/**
 * データベースにシナリオを登録する
 *
 * @param json
 * @param db
 * @param timestamp
 * @param uid
 */
export async function updateScript(obj, db, timestamp, uid, storeUserId) {
  const { id } = obj;
  const srtiptNameRef = db.collection('users').doc(storeUserId).collection('scripts').doc(id);
  const scriptName = {
    name: obj.name, uid, id, updatedAt: timestamp,
  };

  const script = {
    ...obj, id, uid, updatedAt: timestamp,
  };

  return Promise.all([
    (await srtiptNameRef.set(scriptName)),
    (await db.collection('scripts').doc(id).set(script)),
  ]);
}


/**
 * データベースから指定したユーザのシナリオ名一覧を取得する
 *
 * @param db
 * @param storeUserId
 */
export async function readScriptNames(db, storeUserId) {
  const querySnapshot = await db.collection('users').doc(storeUserId).collection('scripts').get();
  const scripts: { id: string, name: string }[] = [];
  await querySnapshot.forEach((doc) => {
    const { id, name } = doc.data();
    scripts.push({ id, name });
  });
  return scripts;
}

/**
 * データベースから指定したユーザのシナリオを取得する
 *
 * @param db
 * @param storeUserId
 */
export async function readScript(db, scriptId) {

  const doc = await db.collection('scripts').doc(scriptId).get();
  console.log(doc)
  return doc.data();
}

/**
 * データベースから指定したユーザのシナリオを削除する
 *
 * @param db
 * @param storeUserId
 */
export async function deleteScript(db, storeUserId, scriptId) {
  try {
    await db.collection('users').doc(storeUserId).collection('scripts').doc(scriptId).delete();
    await db.collection('scripts').doc(scriptId).delete();
    return true;
  } catch (e) {
    console.log(e);
    return false;
  }
}
