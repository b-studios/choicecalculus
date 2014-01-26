package choicecalculus
package lang.javascript

import org.scalatest._
import org.scalatest.matchers.ShouldMatchers._
import org.kiama.util.RegexParserTests

import utility.test

class PrettyPrinterTests extends FlatSpec with test.Helpers {

  import lang.javascript.trees._

  trait Context extends Parser with RegexParserTests {

    import lang.javascript.PrettyPrinter._

    def assertParseAndPrint(input: String) { 
      assertParseAndPrint(input, input)
    }

    def assertParseAndPrint(p: (String, String)) {
      assertParseAndPrint(p._1, p._2)
    }

    def assertParseAndPrint(input: String, expected: String) {
      parseAll(topLevel, input) match {
        case Success(tree, in) if in.atEnd => tree.pretty should be (expected)
        case Success(_, in) => fail(s"extraneous input at ${in.pos}: $input")
        case f => fail(s"parse failure: $f")
      }
    }
  }

  it should "handle ternary expressions with correct associativity" in new Context {

    List(
      ("(foo ? bar : baz) ? bam : bar", "(foo ? bar : baz) ? bam : bar"),
      ("foo ? bar : (baz ? bam : bar)", "foo ? bar : baz ? bam : bar"),
      ("foo ? bar : baz ? bam : bar", "foo ? bar : baz ? bam : bar"),
      ("foo ? (bar ? bam : bar) : boo", "foo ? bar ? bam : bar : boo"),
      ("foo ? bar ? bam : bar : boo", "foo ? bar ? bam : bar : boo"),
      ("3 + 4 ? bam : baz", "3 + 4 ? bam : baz"),
      ("(3 + 4) ? bam : baz", "3 + 4 ? bam : baz"),
      ("3 + (4 ? bam : baz)", "3 + (4 ? bam : baz)"),
      ("1 ? 2 : (3 + 4)", "1 ? 2 : 3 + 4"),
      ("1 ? 2 : 3 + 4", "1 ? 2 : 3 + 4")
    ).foreach(assertParseAndPrint)

  }

  it should "print terms with correct parens" in new Context {

    List(
      "f >>= bar()",
      "(!foo.bar)[baz]",
      "(!foo)().bar",
      "foo.bar.baz()",
      "42 * (1 + 2)",
      "3 + 4[foo]",
      "(3 + 4)[foo]",
      "new foo()",
      "new foo()[bar]",
      "new foo(new A[bar])",
      "new new foo(new A[bar])",
      "a = 42 >>> 43 + 5 << 2 * 3 + 5",
      "\"foo\" + typeof typeof 42 + 4",
      "var x = (foo, bar, baz) + 3"
    ).foreach(assertParseAndPrint)
  }

  it should "wrap top level function expressions in parens" in new Context {

    List(
      """(function () {}.call());
         |functionFoo()""",
         
       """function Foo() {}""")
      .map(_.stripMargin)
      .foreach(assertParseAndPrint)
  }

  it should "print the correct amount of semicolons" in new Context {

    List(
      """while (true) {
        |  return x
        |}""")
      .map(_.stripMargin)
      .foreach(assertParseAndPrint)
  }

  it should "print whitespaces in front of conflicting prefixes" in new Context {
    List(
      "+ ++foo",
      "++ +foo",
      "+ + ++foo",
      "+ +foo",
      "++foo",
      "void 0",
      "typeof Foo",
      "typeof typeof 42",
      "typeof +3",
      "delete typeof foo.bar",
      "+-foo",
      "-+foo",
      "-++foo",
      "- -++foo"
    ).foreach(assertParseAndPrint)
  }

  it should "force blocks around conditionals" in new Context {

    List(
      ("if (foo) x; else if (bar) y; else z", 
       """if (foo) {
         |  x
         |} else if (bar) {
         |  y
         |} else {
         |  z
         |}""")
    )
      .map { case (l,r) => (l, r.stripMargin) }
      .foreach(assertParseAndPrint)
  }

  it should "only add spaces if value is present" in new Context {
    List("return", "break", "continue foo", "return 42 + 43", "break label")
      .foreach(assertParseAndPrint)
  }

  it should "omit initialization if value is `undefined`" in new Context {
    assertParseAndPrint(
      "var foo, bar, baz = undefined, bam = 42",
      "var foo, bar, baz, bam = 42"
    )
  }
}