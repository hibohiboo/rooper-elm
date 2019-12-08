export default class User {
  public uid: string;

  public displayName: string;

  public twitterScreenName: string;

  public twitterProfileImageUrl: string;

  public storeUserId: string;

  constructor(storeUserId: string, {
    uid, displayName, twitterScreenName, twitterProfileImageUrl,
  }) {
    this.uid = uid;
    this.displayName = displayName;
    this.twitterScreenName = twitterScreenName;
    this.twitterProfileImageUrl = twitterProfileImageUrl;
    this.storeUserId = storeUserId;
  }
}

/**
 * データベースからユーザを取得する
 *
 * @param firebaseUser
 * @param db
 * @param timestamp
 */
export async function getUser(db, uid) {
  const usersRef = db.collection('users'); // usersコレクションへの参照を取得
  const query = usersRef.where('uid', '==', uid); // usersコレクションからログインユーザの情報を取得する条件を設定

  const querySnapshot = await query.get(); // ユーザを取得
  if (querySnapshot.size === 0) {
    return false;
  }
  const doc = querySnapshot.docs[0];
  const user = doc.data();
  return new User(doc.id, user);
}

/**
 * データベースにユーザを新規登録する
 *
 * @param firebaseUser
 * @param db
 * @param timestamp
 */
export async function createUser(db, {
  uid = '', displayName = '', twitterScreenName = '', twitterProfileImageUrl = '',
}) {
  const usersRef = db.collection('users'); // usersコレクションへの参照を取得
  const userRef = usersRef.doc();
  const dbuser = {
    uid,
    displayName,
    twitterScreenName,
    twitterProfileImageUrl,
  };
  userRef.set(dbuser);

  return new User(userRef.id, dbuser);
}
