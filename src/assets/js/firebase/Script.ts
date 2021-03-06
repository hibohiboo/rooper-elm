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
  const scriptNameRef = db.collection('users').doc(storeUserId).collection('scripts').doc(id);
  const doc = await scriptNameRef.get();
  const scriptName = {
    name: obj.name, uid, id, updatedAt: timestamp, createdAt: doc.data().createdAt
  };

  const script = {
    ...obj, id, uid, updatedAt: timestamp, createdAt: doc.data().createdAt
  };

  return Promise.all([
    (await scriptNameRef.set(scriptName)),
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
export async function readScript(db, uid, scriptId) {

  const snapshot = await db.collection('scripts').where("uid", "==", uid).where("id", "==", scriptId).get();
  if (snapshot.size === 0) {
    alert("不正な読み込みです。");
    window.location.href = '/';
    return;
  }

  return snapshot.docs[0].data();
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
