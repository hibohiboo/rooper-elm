import {
  FileArchiver,
  convertDocToXML,
  createDoc,
  createElement
} from './fileArchiver';

type Character =
  | 'BoyStudent' //  男子学生
  | 'GirlStudent' //  女子学生
  | 'RichMansDaughter' //  お嬢様
  | 'ShrineMaiden' //  巫女
  | 'PoliceOfficer' //  刑事
  | 'OfficeWorker' //  サラリーマン
  | 'Informer' //  情報屋
  | 'Doctor' //  医者
  | 'Patient' //  患者
  | 'ClassRep' //  委員長
  | 'MysteryBoy' //  イレギュラー
  | 'Alien' //  異世界人
  | 'GodlyBeing' //  神格
  | 'PopIdol' //  アイドル
  | 'Journalist' //  マスコミ
  | 'Boss' //  大物
  | 'Nurse' //  ナース
  | 'Henchman' //  手先
  | 'Scientist' //  学者
  | 'Illusion' //  幻想
  | 'ForensicSpecialist' //  鑑識官
  | 'AI' //  A.I.
  | 'Teacher' //  教師
  | 'TransferStudent' //  転校生
  | 'Soldier' //  軍人
  | 'BlackCat' //  黒猫
  | 'LittleGirl' //  女の子
  | 'Sister' //  妹
  | 'CopyCat' //  コピーキャット
  | 'Guru' //  教祖
  | 'SacredTree'; //  ご神木

export const createZip = (scenario: {
  characters: {
    character: Character;
  }[];
  set: string;
  numberOfLoops: number;
  daysInOneLoop: number;
  extra: string;
  incidents: Incident[];
}) => {
  const files: File[] = [];
  scenario.characters.forEach(c => {
    const file = characterFactory(c.character);
    if (file) {
      files.push(file);
    }
  });
  files.push(createNote(scenario));

  FileArchiver.instance.save(files, 'scenario');
};

const characterFactory = (c: Character) => {
  switch (c) {
    case 'BoyStudent':
      return createCharacter('男子学生', '学校', '01');
    case 'GirlStudent':
      return createCharacter('女子学生', '学校', '02');
    case 'RichMansDaughter':
      return createCharacter('お嬢様', '学校', '03');
    case 'ShrineMaiden':
      return createCharacter('巫女', '神社', '04');
    case 'PoliceOfficer':
      return createCharacter('刑事', '都市', '05');
    case 'OfficeWorker':
      return createCharacter('サラリーマン', '都市', '06');
    case 'Informer':
      return createCharacter('情報屋', '都市', '07');
    case 'Doctor':
      return createCharacter('医者', '病院', '08');
    case 'Patient':
      return createCharacter('患者', '病院', '09');
    case 'ClassRep':
      return createCharacter('委員長', '学校', '10');
    case 'MysteryBoy':
      return createCharacter('イレギュラー', '学校', '11');
    case 'Alien':
      return createCharacter('異世界人', '神社', '12');
    case 'GodlyBeing':
      return createCharacter('神格', '神社', '13');
    case 'PopIdol':
      return createCharacter('アイドル', '都市', '14');
    case 'Journalist':
      return createCharacter('マスコミ', '都市', '15');
    case 'Boss':
      return createCharacter('大物', '都市', '16');
    case 'Nurse':
      return createCharacter('ナース', '病院', '17');
    case 'Henchman':
      return createCharacter('手先', '神社', '18');
    case 'Scientist':
      return createCharacter('学者', '病院', '19');
    case 'Illusion':
      return createCharacter('幻想', '神社', '20');
    case 'ForensicSpecialist':
      return createCharacter('鑑識官', '都市', '21');
    case 'AI':
      return createCharacter('A.I.', '都市', '22');
    case 'Teacher':
      return createCharacter('教師', '学校', '23');
    case 'TransferStudent':
      return createCharacter('転校生', '学校', '24');
    case 'Soldier':
      return createCharacter('軍人', '病院', '25');
    case 'BlackCat':
      return createCharacter('黒猫', '神社', '26');
    case 'LittleGirl':
      return createCharacter('女の子', '学校', '27');
    // 未実装キャラはとりあえず男子学生にする
  }
};

const createCharacter = (charName, firstPosition, cardNumber) => {
  const doc = createDoc();
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
  doc.appendChild(rooperCard);
  const sXML = convertDocToXML(doc);
  return new File([sXML], `${charName}.xml`, { type: 'text/plain' });
};
type Incident = { day: number; incident: string };
const createNote = ({
  set,
  numberOfLoops,
  daysInOneLoop,
  extra,
  incidents
}: {
  set: string;
  numberOfLoops: number;
  daysInOneLoop: number;
  extra: string;
  incidents: Incident[];
}) => {
  const doc = createDoc();
  const textNote = createElement(doc, 'text-note', [
    ['password', ''],
    ['location.x', '210'],
    ['location.y', '150'],
    ['posZ', '0'],
    ['rotate', '0'],
    ['roll', '0'],
    ['zindex', '0'],
    ['location.name', 'table']
  ]);
  const note = createElement(doc, 'data', [['name', 'text-note']]);
  const image = createElement(doc, 'data', [['name', 'image']]);
  const imageIdentifier = createElement(doc, 'data', [
    ['name', 'imageIdentifier'],
    ['type', 'image']
  ]);
  image.appendChild(imageIdentifier);
  note.appendChild(image);

  const common = createElement(doc, 'data', [['name', 'common']]);
  const name = createElement(doc, 'data', [['name', 'title']], '公開シート');
  const height = createElement(doc, 'data', [['name', 'height']], '3');
  const width = createElement(doc, 'data', [['name', 'width']], '6');
  const fontsize = createElement(doc, 'data', [['name', 'fontsize']], '5');
  const text = createElement(
    doc,
    'data',
    [['name', 'text']],
    `${set}
ループ回数: ${numberOfLoops} / 1ループ日数: ${daysInOneLoop}日

${extra}

[事件予定]

${incidents
  .reverse()
  .map(incidentToString)
  .join('\n')}
  `
  );
  common.appendChild(name);
  common.appendChild(height);
  common.appendChild(width);
  common.appendChild(fontsize);
  common.appendChild(text);
  note.appendChild(common);
  const detail = createElement(doc, 'data', [['name', 'detail']]);
  note.appendChild(detail);

  textNote.appendChild(note);
  doc.appendChild(textNote);
  const sXML = convertDocToXML(doc);
  return new File([sXML], `公開シート.xml`, { type: 'text/plain' });
};

const incidentToString = ({ day, incident }: Incident) =>
  `${day}日目：${incident}`;

const incidentFactory = (incident: string) => {
  switch (incident) {
    case 'MurderPlan':
      return '殺人計画';
    case 'LightOfTheAvenger':
      return '復讐者の灯火';
    case 'APlaceToProtect':
      return '守るべき場所';
    case 'TheSealedItem':
      return '封印されしモノ';
    case 'SignWithMe':
      return '僕と契約しようよ！';
    case 'ChangeOfFuture':
      return '未来改変プラン';
    case 'GiantTimeBomb':
      return '巨大時限爆弾Xの存在';
    case 'AnUnsettlingRumour':
      return '不穏な噂';
    case 'AHideousScript':
      return '最低の却本';
    case 'ShadowOfTheRipper':
      return '切り裂き魔の影';
    case 'CircleOfFriends':
      return '友情サークル';
    case 'ALoveAffair':
      return '恋愛風景';

    case '':
      return '';

    case '':
      return '';

    case '':
      return '';

    case '':
      return '';

    case '':
      return '';

    case '':
      return '';

    case '':
      return '';

    case '':
      return '';

    case '':
      return '';

    case '':
      return '';

    case '':
      return '';
  }
};
