/**
 * データベースにシナリオを登録する
 *
 * @param json
 * @param db
 * @param timestamp
 * @param uid
 */
export async function addScenario(scenario, db, timestamp, uid, storeUserId) {
  const userRef = db.collection('users').doc(storeUserId);
  const ref = await userRef.collection('scenarios').doc();
  const { id } = ref;
  const userScenario = {
    name: scenario.name, uid, id, createdAt: timestamp, updatedAt: timestamp,
  };

  const scenario = {
    ...scenario, uid, createUserId: storeUserId, createdAt: timestamp, updatedAt: timestamp,
  };

  return Promise.all([
    (await userRef.collection('scenarios').doc(id).set(userScenario)), // ユーザ画面に部屋一覧を表示するために名前のみ保持
    (await db.collection('scenarios').doc(id).set(scenario)), // 残りの情報は専用のコレクションに保存
  ]);
}

/**
 * データベースから指定したユーザのシナリオ一覧を取得する
 *
 * @param db
 * @param storeUserId
 */
export async function readScenarios(db, storeUserId) {
  const querySnapshot = await db.collection('users').doc(storeUserId).collection('scenarios').get();
  const scenarios: { id: string, name: string }[] = [];
  await querySnapshot.forEach((doc) => {
    const { id, name } = doc.data();
    scenarios.push({ id, name });
  });
  return scenarios;
}
