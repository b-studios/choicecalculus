package choicecalculus
package lang.jscc

import lang.javascript

import org.scalatest._
import org.scalatest.matchers.ShouldMatchers._
import org.kiama.util.RegexParserTests
import utility.test

class ParserTests extends FlatSpec with test.Helpers {

  import javascript.trees.{ BlockStmt, CallExpr, FunctionDecl, GroupExpr, Program,
                            NameAccessExpr, SequenceExpr, ReturnStmt, VarDeclStmt,
                            VarBinding, EmptyStmt }

  import lang.trees.Include

  trait Context extends Parser with RegexParserTests

  it should "not parse identifiers as keywords" in new Context {

    assertParseOk("selector.charAt(0)", expression,
      CallExpr(NameAccessExpr("selector", "charAt"), List(lit("0"))))

    assertParseOk("dimension(a) +  4", expression,
      CallExpr("dimension",List(lit("a"))) + lit("4"))

  }

  it should "parse choice calculus expressions without parenthesis" in new Context {

    assertParseOk("3 + dim A(a) in (4 + 4)", expression,
      lit("3") + dim('A)('a) { GroupExpr(lit("4") + lit("4")) })

    val expected = lit("3") + select('A, 'a, dim('A)('a) { choice('A) ('a -> (lit("4") + lit("5"))) })

    assertParseOk("3 + select A.a from dim A(a) in choice A {\n  case a → 4 + 5\n}", expression, expected)

  }

  it should "parse share expressions" in new Context {
    assertParseOk("share #x:Expression as 4 within #x", expression, share('x, lit("4"), id('x)))
  }

  it should "parse statements as declaration bodies" in new Context {

    assertParseOk("dim A(a) { function Foo() { return 3 } }", statement,
      dim('A)('a) { BlockStmt(List(
        FunctionDecl(lit("Foo"),List(),BlockStmt(List(ReturnStmt(Some(lit("3"))))))))
      })

  }

  it should "parse statements as select bodies" in new Context {

    val toParse = """select A.a from {
      dim A(a) { 3 + 4 }
    }"""

    val expected = select('A, 'a, BlockStmt(List(
      dim('A)('a) { BlockStmt(List(
        lit("3") + lit("4")))
      })))

     assertParseOk(toParse, statement, expected)
     assertParseOk(toParse, topLevel, Program(List(expected)))
     assertParseOk(toParse, parser, Program(List(expected)))
  }

  it should "parse sequence expressions with correct precedence" in new Context {

    assertParseOk("dim A(a) 4, dim B(b) 5", expression,
      dim('A)('a) { SequenceExpr(List(lit("4"),
        dim('B)('b) { lit("5") }))
      })

    assertParseOk("(dim A(a) 4), dim B(b) 5", expression,
      SequenceExpr(List(GroupExpr(
        dim('A)('a) { lit("4") }),
          dim('B)('b) { lit("5") })))
  }

  it should "ignore leading and trailing whitespacess when using strippedPhrase" in new Context {

    val expected = dim('A)('a) { lit("4") }

    assertParseOk("     dim A(a) 4", strippedPhrase(expression), expected)
    assertParseOk("dim A(a) 4     ", strippedPhrase(expression), expected)
    assertParseOk("     dim A(a) 4   ", strippedPhrase(expression), expected)
  }

  it should "parse choicecalculus terms in expression position"  in new Context {

    locally {
      val expected = Program(List(
        dim('A)('a, 'b) {
          FunctionDecl(lit("Foo"),List(),BlockStmt(List(
            ReturnStmt(Some(
              choice('A)(
                'a -> lit("3"),
                'b -> lit("4")))))))
        }))

      assertParseOk("""
        dim A(a,b) in
        function Foo() {
          return choice A {
            case a => 3
            case b => 4
          }
        }
      """, topLevel, expected)
    }

    assertParseOk("#x + #x", expression, id('x) + id('x))
    assertParseOk("#y + #y", assignExpr, id('y) + id('y))
    assertParseOk("(#z + #z)", primExpr, GroupExpr(id('z) + id('z)))

    assertParseOk("#x, #x", expression, SequenceExpr(id('x) :: id('x) :: Nil))
    assertParseOk("var x = #x", declaration,
      VarDeclStmt(VarBinding(lit("x"),id('x)) :: Nil))
  }

  it should "parse choicecalculus terms in statement position" in new Context {

    assertParseOk("""#x""", declaration, id('x))
    assertParseOk("""#y""", statement, id('y))
    assertParseOk("""{#x;#y;#z}""", statement,
      BlockStmt(List(id('x), id('y), id('z))))

    assertParseOk("""dim A(a,b) {
      var x = 42
    }""", declaration, dim('A)('a,'b) {
      BlockStmt(VarDeclStmt(VarBinding(lit("x"),lit("42")) :: Nil) :: Nil)
    })
  }

  it should "parse mixed nested choicecalculus terms and javascript terms" in new Context {

    assertParseOk("""dim A(a,b) {
      var x = #z
    }""", declaration, dim('A)('a,'b) {
      BlockStmt(VarDeclStmt(VarBinding(lit("x"),id('z)) :: Nil) :: Nil)
    })

    assertParseOk("""function Foo() {
      dim A(a,b) {
        var x = #z
      }
    }""", declaration, FunctionDecl(lit("Foo"),List(),BlockStmt(List(
      dim('A)('a,'b) {
        BlockStmt(VarDeclStmt(
          VarBinding(lit("x"), id('z)) :: Nil) :: Nil)
      }
    ))))

  }

  it should "parse include terms" in new Context {

    assertParseOk("""dim A(a,b) {
      include "hello.js"
    }""", declaration, dim('A)('a,'b) {
      BlockStmt(Include("hello.js", statement) :: Nil)
    })

  }

  it should "parse multiple nested dimension declarations" in new Context {

    val expected = dim('A)('a,'b) {
      dim('B)('a,'b) {
        dim('C)('a,'b) {
          GroupExpr(choice('A)(
            'a -> lit("1"),
            'b -> lit("2")
          ) + choice('B)(
            'a -> lit("3"),
            'b -> lit("4")
          ))
        }
      }
    }

    assertParseOk("""
      dim A(a, b) in
      dim B(a, b) in
      dim C(a, b) in
        (choice A {
          case a => 1
          case b => 2
        } + choice B {
          case a => 3
          case b => 4
        })""", strippedPhrase(expression), expected)
  }

  it should "print pretty error messages" in new Context {
    // should yield "[1.18] failure: `(' expected but `*' found"
    parseAll(topLevel, "dim A(a,b) in (3+*4)") match {
      case err:NoSuccess =>
        (err.msg contains "`(' expected but `*' found") should be (true)
    }
  }
}