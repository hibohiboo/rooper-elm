
export async function listenRoomData(db, roomId, ports) {
  const unsubscribe = db.collection("roomsData").doc("roomId")
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

