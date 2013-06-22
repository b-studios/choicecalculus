package choicecalculus
package tests

import org.scalatest._
import org.scalatest.matchers.ShouldMatchers._
import org.kiama.util.RegexParserTests
import choicecalculus.parser.{ Parser }
import choicecalculus.ast.implicits._

class ChoiceCalculusParserTests extends FlatSpec {

  import choicecalculus.ast._
  
  object parser extends Parser with RegexParserTests {
    
    ignore("parse choice expressions with commas inside") {
      
      val expected = ChoiceExpr('A, List( 
          Choice[Expression]('a, "10"),
          Choice[Expression]('b, "15")
      ))
      
      assertParseOk("A<a: 10, b: 15>", expression, expected)
      assertParseOk("A<a: 10, b: 15>", statement, expected)
      
    }
    
    ignore("parse choice expressions as operands") {
      
      val expected = BinaryOpExpr(ChoiceExpr('A, List( 
          Choice[Expression]('a, "10"),
          Choice[Expression]('b, "15")
      )),"+", ChoiceExpr('A, List( 
          Choice[Expression]('a, "10"),
          Choice[Expression]('b, "15")
      )))
      
      assertParseOk("A<a: 10, b: 15> + A<a: 10, b: 15>", expression, expected)
    }
    
    it should "parse choice calculus expressions without parenthesis" in {
      
      assertParseOk("3 + dim A(a) in (4 + 4)", expression, BinaryOpExpr("3","+",
          DimensionExpr('A, 'a :: Nil, GroupExpr(BinaryOpExpr("4","+","4")))))      
      
      val expected = BinaryOpExpr("3","+",
        SelectExpr('A, 'a, 
          DimensionExpr('A, 'a :: Nil,
            ChoiceExpr('A, List(
              Choice[Expression]('a, BinaryOpExpr("4","+","5")))))))
      
      assertParseOk("3 + select A.a from dim A(a) in choice A {\n  case a → 4 + 5\n}", expression, expected)
          
    }
    
    it should "parse share expressions" in {
      
      assertParseOk("share #x:Expression as 4 within #x", expression, ShareExpr('x, Literal("4"), IdExpr('x)))
      
    }
    
  }
  
  parser  
}