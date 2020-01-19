# Syntax of Smopeck Spec Language

## Top Level Definition
```
<TopLevelDef> ::= <TypeDef> | <EndpointDef>
```

### Type Definition
```
<TypeDef> ::= 'type' <TypeName> <TypeVarTuple>? '=' <TypeExp>
<TypeExp> ::= <TypeName> <TypeExtension>? <TypeRefinment>? | <Literal> 
            | <TypeExp> '|' <TypeExp>
            | <TypeExp> '&' <TypeExp>
            | '(' <TypeExp> ')'
<TypeName> ::= r"[A-Z][A-Za-z0-9]*"
<TypeExtension> ::= '{' <TypeBinding>* '}'
<TypeRefinment> ::= '[' <RefineExp> ']'
<TypeVarTuple>  ::= '<' <TypeVar> (',' <TypeVar>)* '>'
<TypeVar> ::= r"[A-Z][A-Za-z0-9]*"

<TypeBinding> ::= <BindingKey> ':' <TypeExp>
<BindingKey> ::= r"[a-z][A-Za-z0-9_\-]*"

<Literal> ::= <DQStringLiteral> | <SQStringLiteral> | <BooleanLiteral> | <NumberLiteral> | <RegexLiteral>
<RefineExp> ::= <RefineAtom> 
              | <RefineExp> '=' <RefineExp>
              | <RefineExp> '=~' <RegexLiteral>
<RefineAtom> ::= '.' | ('.' <Accessor>)+ | <RefineVar>('.' <Accessor>)* | <Literal>
<Accessor> ::= (<TypeName> '#').<BindingKey>
```

### Endpoint Definition
```
<EndpointDef> ::= 'endpoint' <EndpointPath> <Method> <TypeExtension> 
<EndpointPath> ::= Double quoted String Literal
<Method> ::= 'GET' | 'POST' | ... 
```
