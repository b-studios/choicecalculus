package choicecalculus
package tests

import org.scalatest._
import org.scalatest.matchers.ShouldMatchers._
import org.kiama.util.RegexParserTests
import lang.javascript.JavaScriptParser

class JavaScriptParserTests extends FlatSpec with utility.Helpers {
  
  object parser extends JavaScriptParser with RegexParserTests {
    
    it should "parse basic expressions" in {
    
      assertParseOk("foo", expression, lit("foo"))
      assertParseOk("3+4", expression, lit("3") + lit("4"))
    
    }
    
  }
  
  parser  
   
}