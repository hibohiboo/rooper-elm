/**
 * データベースにシナリオを登録する
 *
 * @param json
 * @param db
 * @param timestamp
 * @param uid
 */
export async function addScenario(obj, db, timestamp, uid, storeUserId) {
  const userRef = db.collection('users').doc(storeUserId);
  const ref = await userRef.collection('scenarios').doc();
  const { id } = ref;
  const userScenario = {
    name: obj.name, uid, id, createdAt: timestamp, updatedAt: timestamp,
  };

  const scenario = {
    ...obj, id, uid, createUserId: storeUserId, createdAt: timestamp, updatedAt: timestamp,
  };

  return Promise.all([
    (await userRef.collection('scenarios').doc(id).set(userScenario)), // ユーザ画面に部屋一覧を表示するために名前のみ保持
    (await db.collection('scenarios').doc(id).set(scenario)), // 残りの情報は専用のコレクションに保存
  ]);
}

/**
 * データベースから指定したユーザのシナリオ名一覧を取得する
 *
 * @param db
 * @param storeUserId
 */
export async function readScenarioNames(db, storeUserId) {
  const querySnapshot = await db.collection('users').doc(storeUserId).collection('scenarios').get();
  const scenarios: { id: string, name: string }[] = [];
  await querySnapshot.forEach((doc) => {
    const { id, name } = doc.data();
    scenarios.push({ id, name });
  });
  return scenarios;
}

/**
 * データベースから指定したユーザのシナリオを取得する
 *
 * @param db
 * @param storeUserId
 */
export async function readScenario(db, scenarioId) {

  const doc = await db.collection('scenarios').doc(scenarioId).get();
  console.log(doc)
  return doc.data();
}
