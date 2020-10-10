import {
  FileArchiver,
  convertDocToXML,
  createDoc,
  createElement
} from './fileArchiver';

export const createZip = () => {
  const files: File[] = [];
  const doc = createDoc();
  const card = createCharacter(doc, '男子学生', '学校', '01');
  doc.appendChild(card);
  const sXML = convertDocToXML(doc);
  files.push(new File([sXML], '男子学生.xml', { type: 'text/plain' }));
  // doc.appendChild(card2);

  const doc2 = createDoc();
  const card2 = createCharacter(doc2, '女子学生', '学校', '02');
  doc2.appendChild(card2);
  files.push(
    new File([convertDocToXML(doc2)], '女子学生.xml', { type: 'text/plain' })
  );

  // files.push(new File([sXML], 'data.xml', { type: 'text/plain' }));
  FileArchiver.instance.save(files, 'scenario');
};
const createCharacter = (doc, charName, firstPosition, cardNumber) => {
  const rooperCard = createElement(doc, 'rooper-card', [
    ['location.name', 'table'],
    ['location.x', '600'],
    ['location.y', '950'],
    ['posZ', '0'],
    ['rotate', '0'],
    ['roll', '0'],
    ['zindex', '0'],
    ['state', '0']
  ]);
  // #char
  const char = createElement(doc, 'data', [['name', 'rooper-card']]);
  const image = createElement(doc, 'data', [['name', 'image']]);
  const imageIdentifier = createElement(doc, 'data', [
    ['name', 'imageIdentifier'],
    ['type', 'image']
  ]);
  const front = createElement(
    doc,
    'data',
    [
      ['name', 'front'],
      ['type', 'image']
    ],
    `./assets/images/tragedy_commons_5th/chara_cards/character_${cardNumber}_1.png`
  );
  const back = createElement(
    doc,
    'data',
    [
      ['name', 'back'],
      ['type', 'image']
    ],
    `./assets/images/tragedy_commons_5th/chara_cards/character_${cardNumber}_0.png`
  );
  image.appendChild(imageIdentifier);
  image.appendChild(front);
  image.appendChild(back);
  char.appendChild(image);
  const common = createElement(doc, 'data', [['name', 'common']]);
  const name = createElement(doc, 'data', [['name', 'name']], charName);
  const size = createElement(doc, 'data', [['name', 'size']], '3');
  const pos = createElement(doc, 'data', [['name', '位置']], firstPosition);
  const fPos = createElement(
    doc,
    'data',
    [['name', '初期位置']],
    firstPosition
  );

  const yuko = createElement(
    doc,
    'data',
    [
      ['name', '友好'],
      ['type', 'numberResource'],
      ['currentValue', '0']
    ],
    '0'
  );
  const huan = createElement(
    doc,
    'data',
    [
      ['name', '不安'],
      ['type', 'numberResource'],
      ['currentValue', '0']
    ],
    '0'
  );
  const anyaku = createElement(
    doc,
    'data',
    [
      ['name', '暗躍'],
      ['type', 'numberResource'],
      ['currentValue', '0']
    ],
    '0'
  );
  common.appendChild(name);
  common.appendChild(size);
  common.appendChild(pos);
  common.appendChild(fPos);
  common.appendChild(yuko);
  common.appendChild(huan);
  common.appendChild(anyaku);

  char.appendChild(common);
  const detail = createElement(doc, 'data', [['name', 'detail']]);

  char.appendChild(detail);
  rooperCard.appendChild(char);
  // const cp = createElement(doc, 'chat-palette', [['dicebot', '']]);
  // rooperCard.appendChild(cp);
  return rooperCard;
};
