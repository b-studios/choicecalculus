package choicecalculus
package tests

import org.scalatest._
import org.scalatest.matchers.ShouldMatchers._
import org.kiama.util.Tests

import semantics.Semantics
import org.kiama.util.{ Compiler }
import lang.choicecalculus.ChoiceCalculusParser
import lang.ASTNode
import dimensioning.DimensionGraph
import org.kiama.attribution.Attribution.initTree

class SelectionTests extends FlatSpec {
  
  import lang.javascript.{ Expression, Program, VarDeclStmt, Statement, VarBinding, ReturnStmt }
  
  object interpreter extends Semantics with Compiler[ASTNode] with ChoiceCalculusParser with utility.Helpers {
    
    def substitutionTest(input: ASTNode)(expected: ASTNode) = {
      performSubstitution(input) should equal (expected)
    }
    
    def selectionTest(input: ASTNode)(expected: ASTNode) = {
      performSelection(input) should equal (expected)
    }
    
    def fullReductionTest(input: ASTNode)(expected: ASTNode) = {
      processTree(input) should equal (Right(expected))
    }
    
    it should "find correct dimensioning" in {
      
      // dim A<a> { A<a: 1> }
      val test_dim = dim('A)('a) { choices('A) ('a -> lit("1")) }
      // select A.a from dim A<a> { A<a: 1> }
      val test_selection = Program( select[Statement]('A, 'a, test_dim ) :: Nil)

      val graph_dim = DimensionGraph.empty.fromChoice('A,'a)(test_dim)
                                          .declareDimension('A, 'a :: Nil)(test_dim)
      
      selectionTest(test_selection) { Program(lit("1") :: Nil) } 
      
      dimensioning(test_dim) should equal (graph_dim)
      dimensioning(test_selection) should equal (DimensionGraph.empty)      
    }
    
    it should "not change plain hostlanguage programs" in {
      substitutionTest(lit("foo")) { lit("foo") }
      substitutionTest(lit("3") + lit("5")) { lit("3") + lit("5") }
    }
    
    it should "substitute fully configured shares" in {
      
      // share x = 3+5 in 4 * x
      val share_exp = share('x, lit("3") + lit("5"), lit("4") * id('x))
      
      substitutionTest(share_exp) { lit("4") * (lit("3") + lit("5")) }
      
      dimensioning(share_exp) should equal (DimensionGraph.empty)
      
      dimensioning(share_exp.body).fullyConfigured should equal (true)
      
      // share x = { var foo = 4 } in x
      val share_stmt = share('x, VarDeclStmt(VarBinding(lit("foo"), lit("4")) :: Nil), id('x))
      
      substitutionTest(share_stmt) { VarDeclStmt(VarBinding(lit("foo"), lit("4")) :: Nil) }
    }
    
    it should "first perform selections and then substitute shares" in {
      
     
      // select A.a from 
      //   share x = 3 + 5 in
      //     body
      def shareX(body: Statement): Statement = 
        share('x, lit("3") + lit("5"), body)
      
      // dim A<a, b> {
      //  choice A {
      //    case a => return x
      //    case b => return 2 * x
      //  }
      // }
      val dimA = dim('A)('a, 'b) { 
          choices('A) (
            'a -> ReturnStmt(Some(id('x))),
            'b -> ReturnStmt(Some(lit("2") * id('x)))
          )
        }
        
      // undeclared dimension
      processTree(select('A, 'a, shareX(id('x)))) should be an ('left)
      fullReductionTest(select('A, 'a, shareX( dimA ))) { 
        ReturnStmt(Some(lit("3") + lit("5")))
      }
      fullReductionTest(select('A, 'b, shareX( dimA ))) {
        ReturnStmt(Some(lit("2") * (lit("3") + lit("5"))))
      }
    }
    
    it should "include files and calculate dimensions correctly" in {
      
      // select Config.advanced from 
      //   include "included.cclang"
      val stmt = select('Config, 'advanced, include("examples/include/included.js.cc", statement))
      
      fullReductionTest(stmt) { lit("14") }
    }
    
    it should "detect correctly whether a variable is used or not" in {
      
      val expr = select('Config, 'advanced, id('foo))
      initTree(expr)
      variableIsUsed('foo)(expr) should equal (true)
      variableIsUsed('bar)(expr) should equal (false)
      
    }
    
    it should "reduce multiple nested shares correctly" in {
      
      val share_exp  = share('x, lit("3") + lit("5"), share('y, id('x), id('x) * lit("5")))
      val share_exp2 = share('x, lit("3"), share('y, id('x) * lit("5"), id('y)))
      val share_exp3 = share('x, lit("3"), share('y, lit("5") * id('x), id('y)))
      
      substitutionTest(share_exp) { (lit("3") + lit("5")) * lit("5") }
      
      dimensioning(share_exp) should equal (DimensionGraph.empty)
      
      substitutionTest(share_exp2) { lit("3") * lit("5") }
      substitutionTest(share_exp3) { lit("5") * lit("3")}
    }
    
    it should "perform selections on shared dimensions" in {
      
      val test_dim = dim('A)('a) { choices('A) ('a -> lit("1")) }
      val exp = share('x, test_dim, select('A, 'a, id('x)))
      
      selectionTest(exp) { share('x, test_dim, partialConfig('A -> 'a) (id('x))) }
      fullReductionTest(exp) { lit("1") }
      dimensioning(exp) should equal (DimensionGraph.empty)
    }
    
    it should "perform selections in shared dependend dimensions" in {
      
      val inner_dim = dim('A)('a) { choices('A) ('a -> lit("1")) }
      val outer_dim = dim('B)('b) { choices('B) ('b -> inner_dim) }
      val shared = share('x, outer_dim, select('A, 'a, select('B, 'b, id('x))))
      
      fullReductionTest(shared) { lit("1") }      
    }
    
    // share x = dim A<a> { A<1> + A<1> } in
    //   share y = dim B<b> { 4 * B<select A.a from x> } in
    //     select B.b from y
    it should "perform selections on renamed dimensions" in {
      
      val first_dim = dim('A)('a) {
          choices('A) ('a -> lit("1")) + choices('A) ('a -> lit("1")) 
        }
        
      val second_dim = dim('B)('b) { 
          lit("4") * choices('B) ('b -> select('A, 'a, id('x)))
        }
      val shared = share('x, first_dim, share('y, second_dim, select('B, 'b, id('y))))
      
      fullReductionTest(shared) {  lit("4") * (lit("1") + lit("1")) }      
    }
    
    // select A.a from
    //  select A.b from
    //    dim A(a,b) in choice A {
    //      case a => dim A(b,c) in
    //        choice A {
    //          case b => 3
    //          case c => 7
    //        }
    //      case b => 42
    //      }
    it should "perform inner selections first" in {
      
      val dimBody = dim('A)('a, 'b) {
        choices('A) (
          'a -> dim('A)('b, 'c) {
            choices('A) (
               'b -> lit("3"), 
               'c -> lit("7")
            )
          },
          'b -> lit("42")
        )}
      val selects1 = select('A,'b, select('A,'a, dimBody));
      val selects2 = select('A,'a, select('A,'b, dimBody));
      
      fullReductionTest(selects1) { lit("3") }
      
      // undeclared dimension
      processTree(selects2) should be an ('left)
    }
    
    
    // Examples from the paper
    it should "handle the examples from the vamos2013 paper correctly" in {
      
      // select D.a from 
      //   dim D<a,b> in D<1,2>
      val example3_0 = select('D, 'a, 
          dim('D)('a, 'b) { choices('D) (
            'a -> lit("1"),
            'b -> lit("2")
          )})
      
      fullReductionTest(example3_0) { lit("1") }
      
      // share v = e in
      //   dim X<y, z> in
      //     X<select A.b from v, select A.c from v>
      //
      // with e being something like: dim A<a,b,c> in A<1,2,3>
      val example3_1_e = dim('A)('a, 'b, 'c) { choices('A) (
        'a -> lit("1"),
        'b -> lit("2"),
        'c -> lit("3")
      )}
      
      val example3_1 = share('v, example3_1_e, dim('X)('y, 'z) { choices('X) (
        'y -> select('A, 'b, id('v)),
        'z -> select('A, 'c, id('v))
      )})
      
      val example3_1_sel1 = select('X, 'y, example3_1);
      val example3_1_sel2 = select('X, 'z, example3_1);
      
      fullReductionTest(example3_1_sel1) { lit("2") }
      fullReductionTest(example3_1_sel2) { lit("3") }
      
      // Section 4: Open Questions
      
      // 4.1 Undeclared Dimension
      // Design decision: Not allowed
      val example4_1 = select('D, 't, lit("1") + lit("2"))
      processTree(example4_1) should be an ('left)
      
      // 4.2 Multiple Dimensions
      // Design decision: Not allowed
      val example4_2 = select('D, 'a, 
          dim('D)('a, 'b) { choices('D) (
              'a -> lit("1"),
              'b -> lit("2")
          )} + dim('D)('a, 'c) { choices('D) (
              'a -> lit("3"),
              'c -> lit("4")
          )})
       processTree(example4_2) should be an ('left)
      
      // 4.3 Undeclared Tag
      // Design decision: Not allowed
      val example4_3 = select('D,'a, dim('D)('b, 'c) { choices('D) (
          'b -> lit("1"), 
          'c -> lit("2")
        )})
      val example4_3_2 = select('D,'a, dim('D)('b, 'c) { lit("3") + lit("4") })
      processTree(example4_3_2) should be an ('left)
      
      // 4.4 Dependent Dimensions
      // Design decision: Do not select dependent dimension
      //select B.c from
      //  dim A(a,b) in
      //   choice A {
      //     case a => dim B(c,d) in choice B {
      //       case c => 1
      //       case d => 2
      //     }
      //     case b => 3
      //   }
      val example4_4 = select('B, 'c, dim('A)('a, 'b) {
          choices('A) (
            'a -> dim('B)('c, 'd) { choices('B) (
              'c -> lit("1"),
              'd -> lit("2")
              )},
            'b -> lit("3")
          )})
      processTree(example4_4) should be an ('left)
    }
  }
  
  interpreter
}