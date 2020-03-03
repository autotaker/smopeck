type Pos = Int [ . > 0 ]
type Hoge = String [ . = 'hoge' ]
type Fuga = String [ . = 'fuga' ]
type Main = Object @obj {
    intField1: Pos,
    intField2: Pos,
    numberField: Number [ . > -obj.intField2, . < obj.intField1],
    stringField: String,
    booleanField: Bool,
    nullField: Null,
    arrayField: Array @arr {
        length: Int [ . = 3],
        get(j): Object {
            fst: Int[ . = j + arr.length * 10],
            snd: Hoge | Fuga
        }
    }
}