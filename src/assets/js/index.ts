// import * as M from 'M'; //  tslint-disable-line
import firebaseBackEnd, { hideLoader } from './firebase/FireBaseBackEnd';

import { Elm } from './elm/Main'; //  eslint-disable-line import/no-unresolved

require('../css/styles.scss'); // tslint:disable-line no-var-requires


const initApp = async () => {
  const user = await firebaseBackEnd.getSignedInUser();
  // elmのＤＯＭを作成する元となるＤＯＭ要素
  const mountNode: HTMLElement = document.getElementById('app')!;
  // 初期値を与える
  const { ports } = Elm.Main.init({ node: mountNode, flags: user });
  ports.errorToJs.subscribe((data: string) => console.log(data));
  ports.signOut.subscribe(() => firebaseBackEnd.signOut());
  ports.initLoginUI.subscribe(() => firebaseBackEnd.createLoginUi());

  hideLoader();
};

// 初期設定実行
initApp();
