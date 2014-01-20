Choice Calculus
===============
This project is an implementation of the choice calculus with configuration as presented in [1]. The implementation assumes JavaScript as the base language adding a DSL layer of choice calculus constructs.

## Installation
To install just clone the repository and build the project invoking `sbt compile`. 
The latest builds are also available at the ["builds" branch](https://github.com/b-studios/choicecalculus/tree/builds). 
(For some help on the usage of those jar-files see the notes on the branch's page)

Then create some file containing your source code, like:

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
run --input test.js.cc
```

The following screen will appear:

```
Multiple main classes detected, select one to run:

 [1] choicecalculus.lang.jscc.Compiler
 [2] choicecalculus.lang.jscc.DebugCompiler
 [3] choicecalculus.lang.jscc.Repl
 [4] choicecalculus.lang.lccc.Repl
```

Choose either the `Compiler` or `DebugCompiler` to process your file. If using the
DebugCompiler it is recommended to also write the results to a file like:

```
run --input test.js.cc --output test.html
```

## The Help Screen
For further information on the command line usage please consult the help screen
accessible by:

```
run --help
```

## REPL Usage
You also can use one of the provided REPLs by choosing either `jscc.Repl` or `lccc.Repl`.
The latter one allows entering variational lambda calculus terms with a syntax closer to 
the original choice calculus syntax.

```
> select Name.y from dim Name<x, y> in share #v = Name<x: X, y: Y> in \#v. #v
\Y.Y
```

## The Javascript + Choice Calculus Syntax
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

[1] Martin Erwig, Klaus Ostermann, Tillmann Rendel, and Eric Walkingshaw. 2013. Adding configuration to the choice calculus. In *Proceedings of the Seventh International Workshop on Variability Modelling of Software-intensive Systems* (VaMoS '13). ACM, New York, NY, USA, , Article 13 , 8 pages. DOI=[10.1145/2430502.2430520](http://doi.acm.org/10.1145/2430502.2430520)
