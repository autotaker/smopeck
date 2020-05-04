type Pos = Int [ . > 0 ]
type Hoge = 'hoge' 
type Fuga = 'fuga' 
type Main = Object @obj {
    intField1: Pos,
    intField2: Pos,
    numberField: Number [ . > -obj.intField2, . < obj.intField1],
    stringField: r'[0-9]{10}',
    dqField: "hoge ${obj.stringField} piyo",
    calcField: String [ .= str(obj.intField1 + obj. intField2)],
    booleanField: Bool [ .= obj.intField1 > 40 || obj.intField1 > obj.intField2],
    nullField: Null,
    arrayField: Array @arr {
        length: 3,
        get(j): Object {
            fst: Int[ . = j + arr.length * 10],
            snd: Hoge | Fuga
        }
    }
}