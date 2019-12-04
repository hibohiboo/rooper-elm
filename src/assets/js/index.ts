// import * as M from 'M'; //  tslint-disable-line
import { Elm } from './Main'; //  eslint-disable-line import/no-unresolved

require('../css/styles.scss'); // tslint:disable-line no-var-requires

const flags = {};

// elmのＤＯＭを作成する元となるＤＯＭ要素
const mountNode: HTMLElement = document.getElementById('chatlog')!;

// 初期値を与える
const app = Elm.Main.init({ node: mountNode, flags });
app.ports.errorToJs.subscribe((data: string) => console.log(data));
