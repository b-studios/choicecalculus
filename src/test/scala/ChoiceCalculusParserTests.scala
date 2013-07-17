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
    
    it should "parse statements as declaration bodies" in {
      
      assertParseOk("dim A(a) { function Foo() { return 3 } }", statement,
          DimensionExpr('A,List('a), BlockStmt(List(
            FunctionDecl(Literal("Foo"),List(),BlockStmt(List(ReturnStmt(Some(Literal("3"))))))))))    
      
    }
    
    it should "parse sequence expressions with correct precedence" in {
      
      assertParseOk("dim A(a) 4, dim B(b) 5", expression, 
          DimensionExpr('A, List('a), SequenceExpr(List(Literal("4"), 
            DimensionExpr('B, List('b), Literal("5"))))))
     
      assertParseOk("(dim A(a) 4), dim B(b) 5", expression, 
          SequenceExpr(List(GroupExpr(
            DimensionExpr('A, List('a), Literal("4"))), 
              DimensionExpr('B, List('b), Literal("5")))))
            
    }
    
    it should "ignore leading and trailing whitespacess when using strippedPhrase" in {
      
      val expected = DimensionExpr('A, List('a), Literal("4"))
      
      assertParseOk("     dim A(a) 4", strippedPhrase(expression), expected)
      assertParseOk("dim A(a) 4     ", strippedPhrase(expression), expected)
      assertParseOk("     dim A(a) 4   ", strippedPhrase(expression), expected)
    }
    
  }
  
  parser  
}