package choicecalculus
package tests

import org.scalatest._
import org.scalatest.matchers.ShouldMatchers._
import org.kiama.util.Tests
import choicecalculus.semantics.{ DimensionGraph, Semantics }
import choicecalculus.ast._
import choicecalculus.ast.implicits._
import org.kiama.util.{ Compiler }
import utility.DebugRewriter.{bottomup, attempt, rewrite, test, strategy2utilStrategy}
import choicecalculus.parser.Parser
import org.kiama.attribution.UncachedAttribution.initTree


class SelectionTests extends FlatSpec {
  
  object interpreter extends Semantics with Compiler[ASTNode] with Parser {
    
    def substitutionTest(input: ASTNode)(expected: ASTNode) = {
      performSubstitution(input) should equal (expected)
    }
    
    def selectionTest(input: ASTNode)(expected: ASTNode) = {
      performSelection(input) should equal (expected)
    }
    
    def fullReductionTest(input: ASTNode)(expected: ASTNode) = {
      processTree(input) should equal (expected)
    }
    
    it should "find correct dimensioning" in {
      
      // dim A<a> { A<a: 1> }
      val test_dim = DimensionExpr('A, 'a :: Nil, ChoiceExpr('A, Choice[Expression]('a, "1") :: Nil))
      // select A.a from dim A<a> { A<a: 1> }
      val test_selection = Program(  SelectExpr('A, 'a, test_dim ) :: Nil)
            
      val graph_dim = DimensionGraph.empty.fromChoice('A,'a)(test_dim)
                                          .declareDimension('A, 'a :: Nil)(test_dim)
      
      selectionTest(test_selection) { Program("1" :: Nil) } 
      
      dimensioning(test_dim) should equal (graph_dim)
      dimensioning(test_selection) should equal (DimensionGraph.empty)      
    }
    
    it should "not change plain hostlanguage programs" in {
      substitutionTest("foo") { "foo" }      
      substitutionTest(BinaryOpExpr("3","+","5")) { BinaryOpExpr("3","+","5") }
    }
    
    it should "substitute fully configured shares" in {
      
      // share x = 3+5 in 4 * x
      val share_exp = ShareExpr('x, BinaryOpExpr("3","+","5"), BinaryOpExpr("4", "*", IdExpr('x)))
      
      substitutionTest(share_exp) { BinaryOpExpr("4", "*", BinaryOpExpr("3","+","5")) }
      
      dimensioning(share_exp) should equal (DimensionGraph.empty)
      
      dimensioning(share_exp.body).fullyConfigured should equal (true)
      
      // share x = { var foo = 4 } in x
      val share_stmt = ShareExpr('x, VarDeclStmt(VarBinding("foo", "4") :: Nil), IdExpr('x))
      
      substitutionTest(share_stmt) { VarDeclStmt(VarBinding("foo", "4") :: Nil) }
    }
    
    it should "first perform selections and then substitute shares" in {
      
      
      def select(tag: Symbol, body: Statement): Statement = 
        SelectExpr('A, tag, body)
        
      // select A.a from 
      //   share x = 3 + 5 in
      //     body
      def share(body: Statement): Statement = 
        ShareExpr('x, BinaryOpExpr("3","+","5"), body)
      
      // dim A<a, b> {
      //  choice A {
      //    case a => return x
      //    case b => return 2 * x
      //  }
      // }
      val dim = DimensionExpr('A, List('a, 'b), 
          ChoiceExpr('A, List(
              Choice[Statement]('a, ReturnStmt(Some(IdExpr('x)))),
              Choice[Statement]('b, ReturnStmt(Some(BinaryOpExpr("2", "*", IdExpr('x)))))
          ))
      )
        
      processTree(select('a, share( IdExpr('x) ))) should equal (BinaryOpExpr("3","+","5"))
      processTree(select('a, share( dim ))) should equal { 
        ReturnStmt(Some(BinaryOpExpr("3","+","5")))
      }
      processTree(select('b, share( dim ))) should equal {
        ReturnStmt(Some(BinaryOpExpr("2", "*", BinaryOpExpr("3","+","5"))))
      }
    }
    
    it should "include files and calculate dimensions correctly" in {
      
      // select Config.advanced from 
      //   include "included.cclang"
      val stmt = SelectExpr('Config, 'advanced, 
                  IncludeExpr("examples/include/included.js.cc", statement))
      
      processTree(stmt) should equal { Literal("14") }
    }
    
    it should "detect correctly whether a variable is used or not" in {
      
      val expr = SelectExpr('Config, 'advanced, IdExpr('foo))
      initTree(expr)
      variableIsUsed('foo)(expr) should equal (true)
      variableIsUsed('bar)(expr) should equal (false)
      
      
    }
    
    it should "reduce multiple nested shares correctly" in {
      
      val share_exp = ShareExpr('x,  BinaryOpExpr("3","+","5"), ShareExpr('y, IdExpr('x), BinaryOpExpr(IdExpr('y), "*", "5")))
      val share_exp2 = ShareExpr('x,  Literal("3"), ShareExpr('y, BinaryOpExpr(IdExpr('x), "*", "5"), IdExpr('y)))
      val share_exp3 = ShareExpr('x,  Literal("3"), ShareExpr('y, BinaryOpExpr("5", "*", IdExpr('x)), IdExpr('y)))
      
      substitutionTest(share_exp) { BinaryOpExpr(BinaryOpExpr("3","+","5"), "*", "5") }
      
      dimensioning(share_exp) should equal (DimensionGraph.empty)
      
      substitutionTest(share_exp2) { BinaryOpExpr("3","*", "5") }
      substitutionTest(share_exp3) { BinaryOpExpr("5","*", "3") }
    }
    
    it should "perform selections on shared dimensions" in {
      
      val test_dim = DimensionExpr('A, 'a :: Nil, ChoiceExpr('A, Choice[Expression]('a, "1") :: Nil))
      val exp = ShareExpr('x, test_dim, SelectExpr('A, 'a, IdExpr('x)))
      
      selectionTest(exp) { ShareExpr('x, test_dim, PartialConfig(IdExpr('x), List(('A, 'a)))) }
      fullReductionTest(exp) { Literal("1") }
      dimensioning(exp) should equal (DimensionGraph.empty)
    }
    
    it should "perform selections in shared dependend dimensions" in {
      
      val inner_dim = DimensionExpr('A, 'a :: Nil, ChoiceExpr('A, Choice[Expression]('a, "1") :: Nil))
      val outer_dim = DimensionExpr('B, 'b :: Nil, ChoiceExpr('B, Choice[Expression]('b, inner_dim) :: Nil))
      val share = ShareExpr('x, outer_dim, SelectExpr('A, 'a, SelectExpr('B, 'b, IdExpr('x))))
      
      fullReductionTest(share) { Literal("1") }      
    }
    
    // share x = dim A<a> { A<1> + A<1> } in
    //   share y = dim B<b> { 4 * B<select A.a from x> } in
    //     select B.b from y
    it should "perform selections on renamed dimensions" in {
      
      val first_dim = DimensionExpr('A, 'a :: Nil, BinaryOpExpr(
          ChoiceExpr('A, Choice[Expression]('a, "1") :: Nil), "+", ChoiceExpr('A, Choice[Expression]('a, "1") :: Nil))) 
      val second_dim = DimensionExpr('B, 'b :: Nil, 
          BinaryOpExpr("4", "*", ChoiceExpr('B, Choice[Expression]('b, SelectExpr('A, 'a, IdExpr('x))) :: Nil))
      )
      val share = ShareExpr('x, first_dim, ShareExpr('y, second_dim, SelectExpr('B, 'b, IdExpr('y))))
      
      fullReductionTest(share) {  BinaryOpExpr("4", "*", BinaryOpExpr("1", "+", "1")) }      
    }
  }
  
  interpreter
}