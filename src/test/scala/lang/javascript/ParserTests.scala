package choicecalculus
package lang.javascript

import org.scalatest._
import org.scalatest.matchers.ShouldMatchers._
import org.kiama.util.RegexParserTests

import utility.test

class ParserTests extends FlatSpec with test.Helpers {

  import lang.javascript.trees._

  trait Context extends Parser with RegexParserTests

  it should "parse basic expressions" in new Context {
    assertParseOk("foo", expression, lit("foo"))
    assertParseOk("3+4", expression, lit("3") + lit("4"))
  }

  it should "parse mixed unary and binary operators" in new Context {
    assertParseOk("return typeof this === 'function'", statement, 
      ReturnStmt(Some(BinaryOpExpr( 
        PrefixExpr("typeof", lit("this")), "===", lit("'function'")))))
  }

  it should "parse member access on `this` literal" in new Context {
    assertParseOk("this.bar.baz()", expression, 
      CallExpr(NameAccessExpr(NameAccessExpr(lit("this"), lit("bar")), lit("baz")), Nil))
  }

  it should "parse regular expressions" in new Context {
    assertParseOk("""core_pnum = /[+-]?(?:\d*\.|)\d+(?:[eE][+-]?\d+|)/.source""",
      expression,
      BinaryOpExpr(lit("core_pnum"), "=", 
        NameAccessExpr(lit("""/[+-]?(?:\d*\.|)\d+(?:[eE][+-]?\d+|)/"""), lit("source"))))
  }

  it should "parse empty for-statements" in new Context {
    assertParseOk("for(;;) do_it();", statement,
      ForStmt(None, None, None, CallExpr(lit("do_it"), Nil)))
  }

  it should "parse empty return statements" in new Context {
    assertParseOk("return;", statement, ReturnStmt(None))
    assertParseOk("return ;", statement, ReturnStmt(None))
    assertParseOk("{return\n;}", statement, 
      BlockStmt(ReturnStmt(None) :: Nil))

    assertParseOk("{return \t  42 +\n 41;}", statement, 
      BlockStmt(ReturnStmt(Some(BinaryOpExpr(lit("42"), "+", lit("41")))) :: Nil))
  }

  it should "parse sequence expressions" in new Context {
    assertParseOk("foo, bar, baz", expression, 
      SequenceExpr(lit("foo") :: lit("bar") :: lit("baz") :: Nil))
  }

  it should "parse string literals with escape sequences" in new Context {
    assertParseOk("""("foo\"bar")""", expression, 
      GroupExpr(lit("\"foo\\\"bar\"")))

    assertParseOk("(\"foo\\u1111bar\")", expression,
      GroupExpr(lit("\"foo\\u1111bar\"")))
  }

  it should "parse array literals with arbitrary elements omitted" in new Context {

    assertParseOk("""[1,2,3]""", expression, 
      ArrayExpr(List(lit("1"), lit("2"), lit("3"))))

    assertParseOk("""[,2,3]""", expression, 
      ArrayExpr(List(lit("undefined"), lit("2"), lit("3"))))

    assertParseOk("""[1,,3]""", expression, 
      ArrayExpr(List(lit("1"), lit("undefined"), lit("3"))))

    assertParseOk("""[1,2,]""", expression, 
      ArrayExpr(List(lit("1"), lit("2"), lit("undefined"))))

    assertParseOk("""[1,,]""", expression, 
      ArrayExpr(List(lit("1"), lit("undefined"), lit("undefined"))))

    assertParseOk("""[,,]""", expression, 
      ArrayExpr(List(lit("undefined"), lit("undefined"), lit("undefined"))))

    assertParseOk("""[]""", expression, 
      ArrayExpr(Nil))

  }
}