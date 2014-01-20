Choice Calculus
===============
This branch contains the latest builds of the compilers and REPLs.

On all artifacts the help screen can be invoked by calling

```
java -jar <BUILDNAME>.jar --help
```

### js-cc-...
These are the builds based on JavaScript as a host language. To run the compiler
just enter

```
java -jar js-cc-compiler.jar --input test.js.cc --output test.js
```

### lc-cc
This variation is based on the lambda calculus as host language. You can start 
the REPL session as simply as

```
java -jar lc-cc-repl.jar
```