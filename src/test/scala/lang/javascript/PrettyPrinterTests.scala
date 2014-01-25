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
      parseAll(topLevel, input) match {
        case Success(tree, in) if in.atEnd => tree.pretty should be (input)
        case Success(_, in) => fail(s"extraneous input at ${in.pos}: $input")
        case f => fail(s"parse failure: $f")
      }
    }
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
      "1 ? 2 : 3 + 4",
      "1 ? 2 ? 3 + 4 : 3 + 5 : 3 + 6"
    ).foreach(assertParseAndPrint)
  }

  it should "wrap top level function expressions in parens" in new Context {

    List(
      """(function () {
         |  
         |}.call());
         |functionFoo()""",
         
       """function Foo() {
         |  
         |}""")
      .map(_.stripMargin)
      .foreach(assertParseAndPrint)
  }
}