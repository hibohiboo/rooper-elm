
export async function listenRoomData(db, roomId, ports) {
  const unsubscribe = db.collection("roomsData").doc(roomId)
    .onSnapshot(function (doc) {
      console.log("Current data: ", doc.data());
      ports.readedRoomData.send(doc.data());
    });

  // https://firebase.google.com/docs/firestore/query-data/listen?hl=ja
  // リスナーをデタッチする必要がでたら以下を有効にする
  if (false) {
    unsubscribe();
  }
}

export async function updateRoomData(obj, db, timestamp, uid) {
  const { id } = obj;

  return Promise.all([
    (await db.collection('roomsData').doc(id).set({ ...obj, id, uid, updatedAt: timestamp })),
  ]);
}
