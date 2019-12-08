import * as firebase from 'firebase';
import * as firebaseui from 'firebaseui';
import { createUser, getUser } from './User';

export const hideLoader = () => {
  const activeLoaderClassElement = document.querySelector('.active');
  if (!activeLoaderClassElement) {
    return;
  }
  activeLoaderClassElement.classList.remove('active');
};

export class FireBaseBackEnd {
  public auth;

  public db;

  public firestore;

  /**
   * FireBaseを使用する準備を行う
   * @param {string} name
   * @param {number} age
   * @memberof FireBase
   */
  constructor() {
    // requireは静的必須
    const config = require('./_config'); // eslint-disable-line global-require

    // firebase使用準備
    firebase.initializeApp(config);

    // firebase認証準備
    this.auth = firebase.auth();

    // firestore使用準備
    this.firestore = firebase.firestore;
    this.db = firebase.firestore();

    // this.storage = firebase.storage();
  }

  /**
   * firestoreのタイムスタンプを取得
   */
  public getTimestamp() {
    return this.firestore.FieldValue.serverTimestamp();
  }

  /**
   * サインインしている場合、ユーザを返す
   */
  public getSignedInUser() {
    const { auth, db } = this;
    return new Promise((resolve, reject) => {
      try {
        auth.onAuthStateChanged(
          ({ uid }) => {
            if (!uid) {
              resolve(null);
              return;
            }
            // DBからユーザ情報取得
            (async () => {
              const user = await getUser(db, uid);
              resolve(user);
            })();
          },
        );
      } catch (e) {
        reject(e);
      }
    });
  }

  public async signOut() {
    try {
      await this.auth.signOut();
      return true;
    } catch (e) {
      console.log(`ログアウト時にエラー発生 (${e.message})`);
      return false;
    }
  }

  public createLoginUi() {
    const { db } = this;
    const uiConfig = {
      signInSuccessUrl: '/rooper', // ログイン成功時の遷移先
      signInOptions: [
        firebase.auth.TwitterAuthProvider.PROVIDER_ID,
      ],
      signInFlow: 'redirect',
      callbacks: {
        signInSuccessWithAuthResult(authResult /* , redirectUrl = '/rooper/' */) {
          const { user: { displayName, uid } } = authResult;

          (async () => {
            const user = await getUser(db, uid);
            if (user !== null) {
              console.log('ログイン後ユーザ取得', user);
              // window.location.href = redirectUrl;
              return;
            }
            const twitterUser = authResult.additionalUserInfo;
            const twitterScreenName = twitterUser.profile.screen_name;
            const twitterProfileImageUrl = twitterUser.profile.profile_image_url_https;
            await createUser(db, {
              uid, displayName, twitterScreenName, twitterProfileImageUrl,
            });

            // 手動でリダイレクト
            console.log('ログイン後リダイレクト');
            // window.location.href = redirectUrl;
          })();

          // 手動リダイレクトを待つ
          return false;
        },
        uiShown() {
          // The widget is rendered. Hide the loader.
          hideLoader();
        },
      },
      // Terms of service url. 利用規約。こことプライバシーポリシーのURLをhttps:// からのURLに変えると動かなくなることがある
      tosUrl: '/agreement.html',
      // プライバシーポリシー
      privacyPolicyUrl() {
        window.location.assign('/privacy-policy.html');
      },
    };
    try {
      const ui = new firebaseui.auth.AuthUI(this.auth);
      ui.start('#firebaseui-auth-container', uiConfig);
    } catch (e) {
      // 2回目に読み込んだ時に、elmと競合してfirebaseui-auth-containerの要素が取得できなくなるので、再読み込み
      window.location.reload();
    }
  }
}

export default new FireBaseBackEnd();
