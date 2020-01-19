export const pushHistory = (ports, url) => {
  window.history.pushState({}, '惨劇RoopeR online tool', url);
  // elm に URLの変更を伝える
  ports.changedUrl.send(url);
}


export const historyInit = (ports) => {
  console.log('history init')
  // histroy api 設定.遷移 を防ぐ。aタグは増減するので、親でバブリングしたイベントを受ける。
  //  eslint-disable-next-line no-unused-expressions
  document.querySelector('.rooper-container')?.addEventListener('click', (event) => {
    // aタグ以外は処理をしない
    const { target }: { target: any } = event;

    console.log(target);
    if (target && target.tagName && target.tagName.toUpperCase() !== 'A') {
      return;
    }

    // ドメイン外への遷移は制御しない
    const element = target as HTMLAnchorElement;
    if (element.href.indexOf(document.domain) === -1) {
      return;
    }

    // ドメイントップへの移動は制御しない
    const port = document.location.port ? `:${document.location.port}` : '';
    const topUrl = `${document.location.protocol}//${document.location.hostname}${port}`;
    if (element.href === topUrl || element.href === `${topUrl}/`) {
      return;
    }

    event.preventDefault();
    pushHistory(ports, element.href)
  });

  // ブラウザの戻るボタンを押したときの挙動を設定
  window.addEventListener('popstate', (event) => { console.log(event); ports.changedUrl.send(window.location.href); });

  // elm に 表示時のページURLを伝える
  ports.changedUrl.send(window.location.href);

  // elmからのURL移動を受け取る
  ports.changeUrl.subscribe((url) => {
    const port = document.location.port ? `:${document.location.port}` : '';
    pushHistory(ports, `${document.location.protocol}//${document.location.hostname}${port}${url}`);
  });
};

