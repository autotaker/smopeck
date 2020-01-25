
# Mock アルゴリズム

# Refinementの構文
制約は　`var compOp exp` の形に制限する

```
<RefineStmt> ::= var compOp <RefineExp>
<RefineExp> ::= <Literal> | var | <RefineExp> op <RefineExp> 
```

## 例

```haskell
x = 1
y < (x + 1)
x =~ r"$y *"
message = "Hello $x"
z = sqrt(x*x + y*y) 
```

# Basic Algorithm

## Running Example

```
type Example = Object{
    name : String,
    gender : 'man' | 'woman' | 'other',
    number : r'\d+-\d+-\d'
    age : r'\d+'
    adult : Bool
    summary : String
} [ .greeting =~ r"${.name} [${.gender}, ${.adult}] .*"
  , .adult = int(age) >= 20 ]
```

## Unionをランダムに選択してShapeをMock

```
exampleShape = Object [
    ("name", var1)
  , ("gender", var2)
  , ("number", var3)
  , ("summary", var4)
  , ("age", var5)
  , ("adult", var6)
]
exampleConstraint = [
    var2 = 'man',
    var3 =~ r'\d+-\d+-\d',
    var4 =~ r"${var1} [${$var2}] .*",
    var5 =~ r'\d+',
    var6 = int(var5) >= 20
] 
```

## 依存グラフを作り、依存がないものから変数を生成
依存グラフ

```
var1 -> var4
var2 -> var4
var5 -> var6
var6 -> ver4
```
依存グラフに循環が発生した場合、エラーとする

1. `var1`を生成 `var1 = 'hoge'`
2. `var3`を生成 `var3 = '0123-45-6789'`
3. `var2`を生成 `var2 = 'man'`
4. `var5`を生成 `var5 = '19'`
5. `var6`を生成 `var6 = false`
4. `var4`を生成 `var4 =~ 'hoge [man, false] .*'` これから`var4 = 'hoge [man, false] fuga'`

## 生成した変数割り当てをshapeに適用

```
value = Object [
    ("name", 'hoge'),
    ("gender", 'man'),
    ("number", '0123-45-6789'),
    ("age", '19'),
    ("adult", false),
    ("summary", 'hoge [man, false] fuga')
]
```

# Lazy Mocking Algorithm


## Running Example

```
type Size = Int [ . > 1 ]
type Digits = r"\d+"
type Example = Array<Digits, Size> [ .get(0) = "${.length}" ]
```

## Mocking Shape

```
mocked = Array {
  length = var1,
  get = f
}
constraints:
  - var1 > 1
  - forall 0 <= i < var1. f(i) =~ r"\d+"
  - f(0) = "${var1}"

dependency:
  - var1 -> forall(i)
  - forall(i) -> f
  - var1 -> f(0)
```

## Generate Values

```
assign: [var1 = 3]
constraints:
  - forall 0 <= i < 3. f(i) =~ r"\d+"
  - f(0) = "3"
  
dependency:
  - forall(i) -> f
```

Unfold forall.

```
assign: 
  - var1 = 3
constraints:
  - f(0) =~ r"\d+"
  - f(1) =~ r"\d+"
  - f(2) =~ r"\d+"
  - f(0) = "3"
dependency: []
```

```
assign: 
  - var1 = 3
  - f(0) = "3"
  - f(1) = "123"
  - f(2) = "456"
```

## Apply the assignment to the shape

```
value = Array {
  length = 3,
  get = {
    0: "3"
    1: "123"
    2: "456"
  }
}
```

# Nested ArrayExample 
```
type Size = Int [ . > 1 ]
type Pos = Int [ . > 0 ]
type Example = Array<Array<Pos, Size>, Size> [ .get(0) = .get(1) ]
```

## Mocking Shape
```
shape = Array {
  length = var1
  get = f1
}
constraint:
  - arr.length > 1
  - forall i. 0 <= i < arr.length
    - arr.get(i).length > 1
    - forall j. 0 <= j < arr.get(i).length
      - arr.get(i).get(j) > 0
  - arr.get(0) = arr.get(1)
  
dependency:
  - arr.length -> forall(i)
  - forall(i) -> arr.get(i)
  - arr.get(i) -> arr.get(i).length
  - arr.get(i).length -> forall(i.j)
  - forall(i.j) -> arr.get(i).get(j)
  - arr.get(1) -> arr.get(0)
```

## Solve Constraints

Solving `arr.length`

```
assign:
  - arr.length = 2
constraint:
  - forall i. 0 <= i < 2
    - arr.get(i).length > 1
    - forall j. 0 <= j < arr.get(i).length
      - arr.get(i).get(j) > 0
```

Solving `forall(i)`

```
assign:
  - arr.length = 2
constraint:
  - arr.get(0).length > 1
  - forall j1. 0 <= j1 < arr.get(0).length
    - arr.get(0).get(j1) > 0
  - arr.get(1).length > 1
  - forall j2. 0 <= j2 < arr.get(1).length
    - arr.get(1).get(j2) > 0
dependency:
  - arr.get(0).length -> j1
  - j1 -> arr.get(0).get
  - arr.get(1).length -> j2
  - j2 -> arr.get(1).get
```

Solving `arr.get(1).length`

```
assign:
  - arr.length = 2
  - arr.get(1).length = 2
constraint:
  - arr.get(0).length > 1
  - forall j1. 0 <= j1 < arr.get(0).length
    - arr.get(0).get(j1) > 0
  - forall j2. 0 <= j2 < 2
    - arr.get(1).get(j2) > 0
dependency:
  - arr.get(0).length -> j1
  - j1 -> arr.get(0).get
  - j2 -> arr.get(1).get
```

Solving `forall(j2)`

```
assign:
  - arr.length = 2
  - arr.get(1).length = 2
constraint:
  - arr.get(0).length > 1
  - forall j1. 0 <= j1 < arr.get(0).length
    - arr.get(0).get(j1) > 0
  - arr.get(1).get(0) > 0
  - arr.get(1).get(1) > 0
dependency:
  - arr.get(0).length -> j1
  - j1 -> arr.get(0).get
```

Solving `arr.get(0).length`

```
assign:
  - arr.length = 2
  - arr.get(1).length = 2
  - arr.get(0).length = 2
constraint:
  - arr.get(0).length > 1
  - forall j1. 0 <= j1 < 2
    - arr.get(0).get(j1) > 0
  - arr.get(1).get(0) > 0
  - arr.get(1).get(1) > 0
dependency:
  - j1 -> arr.get(0).get
```

Solving `forall(j2)`

```
assign:
  - arr.length = 2
  - arr.get(1).length = 2
  - arr.get(0).length = 2
constraint:
  - arr.get(0).get(0) > 0
  - arr.get(0).get(1) > 0
  - arr.get(1).get(0) > 0
  - arr.get(1).get(1) > 0
dependency: []
```

Solving the other

```
assign:
  - arr.length = 2
  - arr.get(1).length = 2
  - arr.get(0).length = 2
  - arr.get(0).get(0) = 1
  - arr.get(0).get(1) = 2
  - arr.get(1).get(0) = 3
  - arr.get(1).get(1) = 4
```

Generate value

```
value = [ [1,2], [3,4]]
```

# Algorithm Formalization

- Constraint: これから解くべき制約
- Assignment: すでに解いた制約
- Dependency: 左辺値を解く順序を決めるもの

```
<Constraint> ::= 
  <LHS> : <TypeExp>
  <LHS> op <Exp>
  forall <Exp> <= <var> < <Exp>. <LHS> : <TypeExp>
  
<Dependency> ::=
  <LHS> -> <LHSBlob>
  
<LHS> ::= var | <LHS>.<field> | <LHS>.get(<int>)
<LHSBlob> ::= var | <LHSBlob>.<field> | <LHSBlob>.get(<int>) | | <LHSBlob>.get(*)
  
<Assignment> ::=
  <LHS> = <Value>
```

## Algorithm Step
For each step, the algorithm does either one of the following two actios.
1. If there are no constraints, terminates the algorithm
1. If there is a l-value that is dependency-free, solve the constraints of the l-value.
  - If the l-value is a forall variable, unfold the corresponding forall constraint.
  - Otherwise, gather constraints for the l-value and assign a value that satisfies the constraints.
2. Otherwise, unfold a constraints of the form `<LHS> : <TypeExp>`

Then, update constraint, dependencies and assignments

## Running Example
```
type Example = Object{
    name : String,
    gender : 'man' | 'woman' | 'other',
    number : r'\d+-\d+-\d'
    age : r'\d+'
    adult : Bool
    summary : String
} [ .summary =~ r"${.name} [${.gender}, ${.adult}] .*"
  , .adult = int(age) >= 20 ]

constraints:  
- it : Example
dependency: []

```

 - unfold `it`

```
constraints:
- it.name : String
- it.gender : 'man' | 'woman' | 'other'
- it.number : '\d+-\d+-\d'
- it.age : r'\d+'
- it.adult : Bool
- it.summary : String
- it.summary =~ r"${it.name} [${it.gender}, ${it.adult}] .*"
- it.adult = (int(it.age) >= 20)

dependency:
- it.name -> it.summary
- it.gender -> it.summary
- it.adult -> it.summary
- it.age -> it.adult
```

- solving `it.name` as "autotaker"
- solving `it.number` as "0123-45-6789"
- solving `it.age` as "28"
- solving `it.gender` as 'man'

```
constraints:
- it.adult : Bool
- it.summary : String
- it.summary =~ r"autotaker [man, ${it.adult}] .*"
- it.adult = true

dependency:
- it.adult -> it.summary
```

- solving `it.adult` as `true`

```
constraints:
- it.summary : String
- it.summary =~ r"autotaker [man, true] .*"
```

- solving `it.summary` as `"autotaker [man, true] hello"`

```
constraints: []
assignments:
- it.name = "autotaker"
- it.gender = "man"
- it.age = "28"
- it.number = "0123-45-6789"
- it.adult = true
- it.summary = "autotaker [man, true] hello"
```


