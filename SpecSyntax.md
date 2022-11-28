# Syntax of Smopeck Spec Language

## Top Level Definition
```
<TopLevelDef> ::= <TypeDef> | <EndpointDef>
```

### Type Definition
```
<TypeDef> ::= 'type' <TypeName> '=' <TypeExp>
<TypeExp> ::= <TypeName> <BindAs>? <TypeExtension>? <TypeRefinment>? 
            | <Literal> 
            | <TypeExp> '|' <TypeExp>  -- Union type
            | <TypeExp> '&' <TypeExp>  -- intersection type
            | '(' <TypeExp> ')'

<TypeName> ::= r"[A-Z][A-Za-z0-9]*"
<TypeExtension> ::= '{' <TypeBindingList> '}'
<TypeRefinment> ::= '[' <RefineList> ']'

<BindAs> ::= '@' <LowerId>

<TypeBindingList> ::= <TypeBinding> (',' <TypeBinding>)*


<TypeBinding> ::= <BindingKey> ':' <TypeExp>
<BindingKey> ::= r"[a-z][A-Za-z0-9_\-]*"

<Literal> ::= <DQStringLiteral> | <SQStringLiteral> | <BooleanLiteral> | <NumberLiteral> | <RegexLiteral>
<RefineList> ::= <RefineExp> (',' <RefineExp>)*
<RefineExp> ::= '.' <CompOp> <Exp>
<RefineAtom> ::= <RefineVar>('.' <Accessor>)* | <Literal>
<Accessor> ::= (<TypeName> '#')? <BindingKey> | 'get(' <Exp> ')'
```

### Endpoint Definition
```
<EndpointDef> ::= 'endpoint' <EndpointPath> <Method> <TypeExtension> 
<EndpointPath> ::= Double quoted String Literal
<Method> ::= 'GET' | 'POST' | ... 
```

## Conditional Types
You can write conditional type with the following syntax.

```
<TypeExp> ::= <TypExp> '?' <Exp>
```

`ty ? e` is read as "this type accepts a value of `ty` if the condition `e` is true, and otherwise this type has no inhabitans".

## Optional Fields
Optional fields for objects are written with the following syntax.

```
<OptionaAssoc> ::= <Field> '?:' <TypeExp>
```

For example,

```
Object @ obj{
    intField : Int
    optionalField ?: Object {
        key: String,
        size: Int,
        ord: 'asc' | 'desc'
    }
}
```

Optinal fields are accessible with the following accessors 
`obj.optionalField?.key`.
This will introduce monadic computation.
In monadic computation, optional failures are ignored.

For example `Int[ . > obj.optionalField?.size ]` is equal to
`Int [ . > 10 ]` for object `{ optionalField : { size : 10 }}`,
and is equal to `Int []` if `optinalField` is not set.

You can set default value for optinal computation with `??` operator.
`Int [ . > (obj.optinalField?.size + 1) ?? 3 ]` is equal to
`Int [ . > 4 ]` if `optinalField` is not set.
