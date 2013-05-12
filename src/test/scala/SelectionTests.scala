package choicecalculus
package tests
/*
import org.scalatest._
import org.scalatest.matchers.ShouldMatchers._
import org.kiama.util.Tests
import semantics.{ DimensionGraph, Semantics }
import ast._
import org.kiama.util.{ Compiler }
import org.kiama.rewriting.Rewriter.{rewrite}

class SelectionTests extends FlatSpec {

  object interpreter extends Semantics with Compiler[ASTNode] {
    
    import choicecalculus.parser.builders._
    
    // not needed here, but required by Compiler
    def parser = null
    
    it should "find correct dimensioning" in {
      
      // use fixtures for this
      val test_dim = exprBuilder.dimension('A, 'a :: Nil, exprBuilder.choice('A, Choice[Expression]('a, Literal("1")) :: Nil))
      val test_selection = Program(  stmtBuilder.select('A, 'a, test_dim ) :: Nil)
      
      val graph_dim = DimensionGraph.empty.fromChoice('A,'a)(test_dim)
                                          .declareDimension('A, 'a :: Nil)(test_dim)
      
      dimensioning(test_dim) should equal (graph_dim)
      dimensioning(test_selection) should equal (DimensionGraph.empty)
      
      rewrite (select) (test_selection) should equal (Program(Literal("1") :: Nil))
    }
    
  }
  
  interpreter
}*/