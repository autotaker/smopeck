# Syntax of Smopeck Spec Language

## Top Level Definition
```
<TopLevelDef> ::= <TypeDef> | <EndpointDef>
```

### Type Definition
```
<TypeDef> ::= 'type' <TypeName> '=' <TypeExp>
<TypeExp> ::= <TypeName> <TypeExtension>? <TypeRefinment>? 
            | <Literal> 
            | <TypeExp> '|' <TypeExp>  -- Union type
            | <TypeExp> '&' <TypeExp>  -- intersection type
            | '(' <TypeExp> ')'

<TypeName> ::= r"[A-Z][A-Za-z0-9]*"
<TypeExtension> ::= '{' <TypeBindingList> '}'
<TypeRefinment> ::= '[' <RefineList> ']'

<TypeBindingList> ::= <TypeBinding> (',' <TypeBinding>)*


<TypeBinding> ::= <BindingKey> ':' <TypeExp>
<BindingKey> ::= r"[a-z][A-Za-z0-9_\-]*"

<Literal> ::= <DQStringLiteral> | <SQStringLiteral> | <BooleanLiteral> | <NumberLiteral> | <RegexLiteral>
<RefineList> ::= <RefineExp> (',' <RefineExp>)*
<RefineExp> ::= <RefineAtom> 
              | <RefineExp> '=' <RefineExp>
              | <RefineExp> '=~' <RegexLiteral>
<RefineAtom> ::= '.' | ('.' <Accessor>)+ | <RefineVar>('.' <Accessor>)* | <Literal>
<Accessor> ::= (<TypeName> '#') <BindingKey>
```

### Endpoint Definition
```
<EndpointDef> ::= 'endpoint' <EndpointPath> <Method> <TypeExtension> 
<EndpointPath> ::= Double quoted String Literal
<Method> ::= 'GET' | 'POST' | ... 
```
