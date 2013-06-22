Choice Calculus
===============
This project is an implementation of the choice calculus with configuration as presented in [1]. The implementation assumes JavaScript as the base language adding a DSL layer of choice calculus constructs.

## Installation
To install just clone the repository and build the project using sbt. Then create some file containing your source code, like:

```
// test.js.cc
dim A(a,b) in
  choice A {
    case a => 1
    case b => 2
  } 
```
Enter sbt and process your file by typing
```
run "test.js.cc"
```

## The Syntax
To fit into JavaScript the syntax differs a little bit from the one used in [1]. The following examples illustrate the differences.

All choice calculus terms can appear in statement as well as in expression position. Allowing the calculus to be used at different levels in the program. This context then also applies to the `BODY` of a choice calculus term and such rendering `3 + dim A(a) in { f(); g(); }` syntactically incorrect since the context is in expression position whereas the body is a `BlockStatement`.

Note: To prevent parsing ambiguities in expression context `BODY` often has to be wrapped in parenthesis.

### Dimension Declaration
```
dim A(a,b,c) in BODY
```

### Choice
```
choice A {
  case a => BODY
  case b => BODY
  case c => BODY
}
```
Choices are statically bound to the next matching lexically surrounding dimension declaration.

### Selection
```
select A.a from BODY
```

### Sharing
```
share #id:CONTEXT as BODY within BODY
```
Here `CONTEXT` can either be `Expression` or `Statement` and specifies which parser rule to use for the first `BODY`. The id expression `#id` can then be used in the second BODY and will be bound to the first one.

### File Includes
```
include "filename"
```
Depending on the context in which `include` appears determines how the file is processed. So using `include` in expression position like `3 + include "foo"` requires the file foo to only contain an expression.
