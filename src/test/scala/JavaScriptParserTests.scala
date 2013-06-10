package choicecalculus
package tests

import org.scalatest._
import org.scalatest.matchers.ShouldMatchers._
import org.kiama.util.RegexParserTests
import choicecalculus.parser.Parser

class JavaScriptParserTests extends FlatSpec {

  import ast._
  
  object parser extends Parser with RegexParserTests {
    
    it should "parse basic expressions" in {
    
      assertParseOk("foo", expression, Literal("foo"))
      assertParseOk("3+4", expression, BinaryOpExpr(Literal("3"), "+", Literal("4")))
    
    }
    
  }
  
  parser  
  
  
}