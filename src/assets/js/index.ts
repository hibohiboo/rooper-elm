// import * as M from 'M'; //  tslint-disable-line
import firebaseBackEnd, { hideLoader } from './firebase/FireBaseBackEnd';

import { Elm } from './elm/Main'; //  eslint-disable-line import/no-unresolved

require('../css/styles.scss'); // tslint:disable-line no-var-requires


const initApp = async () => {
  const user = await firebaseBackEnd.getSignedInUser();
  // ユーザが取得できない場合、ログイン画面を表示
  if (user === null) {
    firebaseBackEnd.createLoginUi();
    return;
  }
  console.log('ログインユーザ', user);

  // // サインアウトのイベントを作成
  // const singOut = document.getElementById('sign-out')!;
  // singOut.addEventListener('click', () => {
  //   firebaseBackEnd.signOut();
  // });
  const flags = JSON.stringify(user);

  // elmのＤＯＭを作成する元となるＤＯＭ要素
  const mountNode: HTMLElement = document.getElementById('app')!;
  console.log('mount', mountNode);
  console.log('flags', flags);
  // 初期値を与える
  const { ports } = Elm.Main.init({ node: mountNode, flags: user });
  ports.errorToJs.subscribe((data: string) => console.log(data));

  hideLoader();
};

// 初期設定実行
initApp();
