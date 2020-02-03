
# Mocking Algorithm

# Algorithm Formalization

## Data Types


### Symbols
A *symbol* is used to describe the address of some location. 
A *symbol* blob is a pattern that matches a set of symbols.

```
<Symbol> ::= var | <Symbol>.<Field> | <Symbol>.get(<IntegerOrVar>)

<SymbolBlob> ::= var | <SymbolBlob>.<Field> | <Symbol>.get(<IntegerOrBlob>)
```

### Values
A *value* represent the data stored at symbol. 
A value is constructed *lazily* for objects and arrays, that is
an object value is a finite binding from fields to symbols (not to values) and
an array value is a special object that has the `length` field and parameterized `get(i)` field.

An *assignment* is a finite map from symbols to values.

```
<Value> ::= <BoolValue> | <NumberValue> | <NullValue> | <StringValue> | <ObjectValue> | <ArrayValue>
<BoolValue> ::= true | false
<NumberValue> ::= Real Numbers
<NullValue> ::= null
<StringValue> ::= Strings
<ObjectValue> ::= { (<Field> : <Symbol>)* }
<ArrayValue> ::= { length : <Symbol>, forall 0 < i <= <Symbol>. get(i) : <SymValue> }

<Assignment> ::= { (<Symbol> := <Value>)* }
```


### Constraints
*Constraints* denotes the restrictions symbols must satisfy.
There are three forms of constraints. 

```
<Constraint> ::= <Symbol> : <TypeExp> 
               | <Symbol> <CompOp> <Exp>
               | forall 0 <= var < <Symbol>. <Constraint>
```

Example

```
it : Number [ . <  other ] -- depends on other
it : Object {..} [ .name < other] -- it does not depend on other
it : Object {..} [ . = other ] -- it does not depend on other

```


Normalized constraint

```
<NConstraint> ::= <Location> : <NType> [ <DNFFormula>, <Assertion> ]
                | forall <Location> in [<Exp> .. <Exp>]. <TypeExp>
<DNFFormula> :: <Clause> ('||' <Clause>)*
<Clause> ::= <Formula> ('&&' <Formula>)*
<Assertion> ::= <Exp>
<Formula> ::=  . <CompOp> <Exp>
<NType> ::= SNull | SNumber | SString 
         | SObject@it { field_1 : typeExp_1, ..., field_n : typeExp_n }
         | SArray@it { length : typeExp_1, get(i) : typeExp_2 }

type Example = Array@a {
  length : Pos [ . < 10]
  get(i) : Number [ . = a.length - i ]
}
it.length : Number [ . > 0 && . < 10]
get(i) : SNumber [ . = it.length - i]

type Example2 = Array@a {
  length : Pos,
  get(i) : Array@b {
    length : Pos [ . > i + 1],
    get(j) : [ . > a.length + a.get(a.length - 1 - i).length + j ]
  }
}

it.length : Pos
forall i . it.get(i) : [ . > it.length + it.get(it.length - 1 - i).length + j ]

```

### Dependency
*Dependencies* are directed edges between symbols, which is used to
determine the order to solve constraints.
A set dependencies is represented as an arrow from `<Symbol> -> <SymbolBlob>`, which 
means all symbols that matches the RHS depends on the LHS.

```
<Dependency> ::= <Symbol> -> <SymbolBlob>
```

## Algorithms

### Main Algorihm
- Input
  - Type Definitions `Def`
  - Target type `tyExp`
- Output
  - JSON value `v` that satisfies `v : TypeExp`
- Algorithm
  - initialize variables
    - `Cs := { it : tyExp }`
    - `Ds := []`
    - `V := { it }`
    - `A := {}`
  - until `Cs` will be empty
    - `G := calculate dependency(V, Ds)`
    - `s := FindDepFreeSymbol(G, dom(A))`
    - Let `C` be the set of constraints in `Cs` whose LHS is `s`.
    - `v, Cs', Ds', V' := SolveConstraints(A, s, C)`
    - update variables
      - `V := V' \cup V`
      - `Cs := Cs \ C \cup Cs'`
      - `Ds := Ds \cup Ds'`
      - `A[s] := v`
  - return `eval(A, it)`
  

### Calculate Dependency
- Input
  - Set of Symbols `V`
  - Dependency `D`
- Output
  - Dependency Graph `G = (V, E)`
  
- Algorithm
  - Iterate until `E` is saturated
    - for each symbol of the form `s.field` in `V`:
      - add edge `s -> s.field` to `E`
    - for each dependency `from -> toBlob`:
       - for each symbol `s` in `V`:
         - if `toBlob` matches `s`: add edge `from -> s` to `E`

### Find a dependency-free symbol
- Input
  - Dependency Graph `G = (V, E)`
  - Set of already assigned symbols `S`
- Output
  - a symbol `s` such that `prev_G(s)` in  `S`
  - if there are no such symbol, return `null`
  
### Solve constraints
- Input
  - Assignments `A`
  - Symbol `s`
  - Constraints `C` whose LHS are equal to `s`.
- Output
  - a value of `s` that satisifes `C` under `A`
  - Additional constraints `Cs`
  - Additional dependencies `Ds`
- Algorithm
  - Normalize `C` to `s : Prim_1 ext_1 ref_1 | ... | Prim_n ext_n ref_n `
  - Solve each `s : Prim_i ext_i ref_i`, case analysis of `Prim_i`
     - case `Number`
       - `ext_i` must be empty
       - solve `ref_i` by using Range logic
     - case `String`
       - `ext_i` must be empty
       - solve `ref_i` by using Regular Expression logic
     - case `Bool` | case `Null` 
       - try all possible values `true`, `false`, or `null`.
     - case `Object`
       - `ext_i` is of the form `{ field_1 : tyExp_1, ..., field_k : tyExp_k }`
       - Assign as `Object{ field_1 : s.field_1 ... , field_k : s.field_k }` according to `ext_i`.
       - Additional constraints
         - `s.field_1 : tyExp_1` ...   `s.field_n : tyExp_n`
         - `s.accessor_i op exp_i` for each `.accessor_i op exp_i` in `ref_i`
         - `s.field_1 = exp.field_1` ... `s.field_k = exp.field_k` for each `. = exp` in `ref_i`
       - Additional dependencies
         - `sym -> s.accessor_i` for each symbol `sym` occurs in `exp_i` for each `s.accessor_i op exp_i` in additional constraints
     - case `Array`
       - `ext_i` must be of the form `{ length : tyExp1, elements : tyExp2 }`
       - Assign as `Array{ length : s.length, forall 0 <= ix < s.length. get(ix) : tyExp2 }` where `ix` is a fresh symbol.
       - Additional constraints
         - `s.length : tyExp1`
         - `forall 0 <= ix < s.length. s.get(ix) : tyExp2`
         - `s.accessor_i op exp_i` for each `.accessor_i op exp_i` in `ref_i`
         - `s.length = exp.length`, `forall 0 <= ix' < s.length. s.get(ix') = exp.get(ix')` for each `. = exp` in `ref_i`
       - Additional dependencies
         - `s.length -> ix` and `ix -> s.get(*)` for each additional constraint of the form `forall 0 <= ix < s.length. s.get(ix) ... ` 
       
     
     
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
