// import * as M from 'M'; //  tslint-disable-line
import firebaseBackEnd from './firebase/FireBaseBackEnd';

import { Elm } from './Main'; //  eslint-disable-line import/no-unresolved

require('../css/styles.scss'); // tslint:disable-line no-var-requires


const initApp = async () => {
  const user = await firebaseBackEnd.getSignedInUser();

  // ユーザが取得できない場合、ログイン画面を表示
  if (user === null) {
    firebaseBackEnd.createLoginUi();
    return;
  }
  const flags = {};

  // elmのＤＯＭを作成する元となるＤＯＭ要素
  const mountNode: HTMLElement = document.getElementById('chatlog')!;
  console.log('mount', mountNode);

  // 初期値を与える
  const app = Elm.Main.init({ node: mountNode, flags });
  app.ports.errorToJs.subscribe((data: string) => console.log(data));
};

// 初期設定実行
initApp();
