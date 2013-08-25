package choicecalculus
package tests

import org.scalatest._
import org.scalatest.matchers.ShouldMatchers._
import org.kiama.util.RegexParserTests
import lang.choicecalculus.ChoiceCalculusParser
import lang.javascript.implicits._

class ChoiceCalculusParserTests extends FlatSpec with utility.Helpers {

  import lang.javascript.{ BlockStmt, CallExpr, FunctionDecl, GroupExpr, Program, NameAccessExpr, SequenceExpr, ReturnStmt }
  
  object parser extends ChoiceCalculusParser with RegexParserTests {
    
    ignore("parse choice expressions with commas inside") {
      
      val expected = choices('A) ( 
        'a -> lit("10"),
        'b -> lit("15")
      )
      
      assertParseOk("A<a: 10, b: 15>", expression, expected)
      assertParseOk("A<a: 10, b: 15>", statement, expected)
      
    }
    
    ignore("parse choice expressions as operands") {
      
      val expected = choices('A) (
        'a -> lit("10"),
        'b -> lit("15")
      ) + choices('A) ( 
        'a -> lit("10"),
        'b -> lit("15")
      )
      
      assertParseOk("A<a: 10, b: 15> + A<a: 10, b: 15>", expression, expected)
    }
    
    it should "not parse identifiers as keywords" in {
      
      assertParseOk("selector.charAt(0)", expression, 
          CallExpr(NameAccessExpr("selector", "charAt"), List(lit("0"))))
      
      assertParseOk("dimension(a) +  4", expression, 
          CallExpr("dimension",List(lit("a"))) + lit("4"))
      
    }
    
    it should "parse choice calculus expressions without parenthesis" in {
      
      assertParseOk("3 + dim A(a) in (4 + 4)", expression, 
          lit("3") + dim('A)('a) { GroupExpr(lit("4") + lit("4")) })
      
      val expected = lit("3") + select('A, 'a, dim('A)('a) { choices('A) ('a -> (lit("4") + lit("5"))) })
      
      assertParseOk("3 + select A.a from dim A(a) in choice A {\n  case a → 4 + 5\n}", expression, expected)
          
    }
    
    it should "parse share expressions" in {
      assertParseOk("share #x:Expression as 4 within #x", expression, share('x, lit("4"), id('x)))
    }
    
    it should "parse statements as declaration bodies" in {
      
      assertParseOk("dim A(a) { function Foo() { return 3 } }", statement,
          dim('A)('a) { BlockStmt(List(
            FunctionDecl(lit("Foo"),List(),BlockStmt(List(ReturnStmt(Some(lit("3"))))))))
          })
      
    }
    
    it should "parse statements as select bodies" in {
      
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
    
    it should "parse sequence expressions with correct precedence" in {
      
      assertParseOk("dim A(a) 4, dim B(b) 5", expression, 
        dim('A)('a) { SequenceExpr(List(lit("4"), 
          dim('B)('b) { lit("5") })) 
        })
     
      assertParseOk("(dim A(a) 4), dim B(b) 5", expression, 
          SequenceExpr(List(GroupExpr(
            dim('A)('a) { lit("4") }), 
              dim('B)('b) { lit("5") })))
            
    }
    
    it should "ignore leading and trailing whitespacess when using strippedPhrase" in {
      
      val expected = dim('A)('a) { lit("4") }
      
      assertParseOk("     dim A(a) 4", strippedPhrase(expression), expected)
      assertParseOk("dim A(a) 4     ", strippedPhrase(expression), expected)
      assertParseOk("     dim A(a) 4   ", strippedPhrase(expression), expected)
    }
    
  }
  
  parser  
}