package choicecalculus
package phases
package evaluator

import org.scalatest._

import org.kiama.util.Tests

import lang.ASTNode
import lang.JsCcParser

import org.kiama.attribution.Attribution.initTree
import org.kiama.rewriting.Rewriter.rewrite

import dimensionchecker.DimensionGraph

import utility.test
import utility.messages._ //{ mute, resetMessages, hasBeenReported, FatalPhaseError, 
                         // Level }

class SelectionTests extends FlatSpec with matchers.ShouldMatchers {

  import lang.javascript.{ Expression, Program, VarDeclStmt, Statement, VarBinding, ReturnStmt }

  /**
   * this is called `evaluating` in scala test 1.9
   *
   * Can be removed after migrating to scala test 2.0
   */
  def running[T](block: => T) = evaluating(block)

  object evaluator extends Evaluator with Reader with Parser with Namer
    with DimensionChecker with JsCcParser with test.Helpers {

    /**
     * In order to perform selection the Reader-, Namer- and DimensionCheckerphase
     * have to be run.
     */
    def selecting(tree: ASTNode): ASTNode = {
      resetMessages()
      runReader(tree)
      runNamer(tree)
      runDimensionChecker(tree)
      rewrite(reduce(select))(tree)
    }

    def substituting(tree: ASTNode): ASTNode = {
      resetMessages()
      runReader(tree)
      runNamer(tree)
      runDimensionChecker(tree)
      rewrite(reduce(substitute + removeShares))(tree)
    }

    def evaluating(tree: ASTNode): ASTNode = {
      resetMessages()
      runReader(tree)
      runNamer(tree)
      runDimensionChecker(tree)
      runEvaluator(tree)
    }

    def vacuousWarning = hasBeenReported { msg => 
      msg.phase == 'dimensionchecker && 
      msg.level == Level.Warn &&
      (msg.message contains "vacuous")
    }

    it should "find correct dimensioning" in {

      // dim A<a> { A<a: 1> }
      val test_dim = dim('A)('a) { choice('A) ('a -> lit("1")) }
      // select A.a from dim A<a> { A<a: 1> }
      val test_selection = Program( select[Statement]('A, 'a, test_dim ) :: Nil)

      val graph_dim = DimensionGraph.empty.fromChoice('A,'a)(test_dim)
                                          .declareDimension('A, 'a :: Nil)(test_dim)

      selecting(test_selection) should equal {
        Program(lit("1") :: Nil)
      }

      mute {
        dimensioning(test_dim) should equal (graph_dim)
        dimensioning(test_selection) should equal (DimensionGraph.empty)
      }
    }

    it should "not change plain hostlanguage programs" in {
      substituting(lit("foo")) should equal {
        lit("foo")
      }
      substituting(lit("3") + lit("5")) should equal {
        lit("3") + lit("5")
      }
    }

    it should "substitute fully configured shares" in {

      // share x = 3+5 in 4 * x
      val share_exp = share('x, lit("3") + lit("5"), lit("4") * id('x))

      substituting(share_exp) should equal {
        lit("4") * (lit("3") + lit("5"))
      }

      mute {
        dimensioning(share_exp) should equal (DimensionGraph.empty)
        dimensioning(share_exp.body).fullyConfigured should equal (true)
      }

      // share x = { var foo = 4 } in x
      val share_stmt = share('x, VarDeclStmt(VarBinding(lit("foo"), lit("4")) :: Nil), id('x))

      substituting(share_stmt) should equal {
        VarDeclStmt(VarBinding(lit("foo"), lit("4")) :: Nil)
      }
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
          choice('A) (
            'a -> ReturnStmt(Some(id('x))),
            'b -> ReturnStmt(Some(lit("2") * id('x)))
          )
        }

      // undeclared dimension
      evaluating(select('A, 'a, shareX(id('x))))
      vacuousWarning should be (true)

      evaluating(select('A, 'a, shareX( dimA ))) should equal {
        ReturnStmt(Some(lit("3") + lit("5")))
      }

      evaluating(select('A, 'b, shareX( dimA ))) should equal {
        ReturnStmt(Some(lit("2") * (lit("3") + lit("5"))))
      }
    }


    // TODO create own test section on includes
    it should "include files and calculate dimensions correctly" in {

      // select Config.advanced from
      //   include "included.cclang"
      val stmt = select('Config, 'advanced, include("examples/include/included.js.cc", parsers.topLevel))

      evaluating(stmt) should equal { Program(lit("14") :: Nil) }
    }

    // Move in separate testfile on 'substitution'
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

      substituting(share_exp) should equal {
        (lit("3") + lit("5")) * lit("5")
      }

      mute {
        dimensioning(share_exp) should equal (DimensionGraph.empty)
      }

      substituting(share_exp2) should equal { lit("3") * lit("5") }
      substituting(share_exp3) should equal { lit("5") * lit("3") }
    }

    it should "perform selections on shared dimensions" in {

      val test_dim = dim('A)('a) { choice('A) ('a -> lit("1")) }
      val exp = share('x, test_dim, select('A, 'a, id('x)))

      selecting(exp) should be {
        share('x, test_dim, partialConfig('A -> 'a) (id('x)))
      }
      evaluating(exp) should equal { lit("1") }
      
      mute {
        dimensioning(exp) should equal (DimensionGraph.empty)
      }
    }

    it should "perform selections in shared dependend dimensions" in {

      val inner_dim = dim('A)('a) { choice('A) ('a -> lit("1")) }
      val outer_dim = dim('B)('b) { choice('B) ('b -> inner_dim) }
      val shared = share('x, outer_dim, select('A, 'a, select('B, 'b, id('x))))

      evaluating(shared) should equal { lit("1") }
    }

    // share x = dim A<a> { A<1> + A<1> } in
    //   share y = dim B<b> { 4 * B<select A.a from x> } in
    //     select B.b from y
    it should "perform selections on renamed dimensions" in {

      val first_dim = dim('A)('a) {
          choice('A) ('a -> lit("1")) + choice('A) ('a -> lit("1"))
        }

      val second_dim = dim('B)('b) {
          lit("4") * choice('B) ('b -> select('A, 'a, id('x)))
        }

      val shared = share('x, first_dim, share('y, second_dim, select('B, 'b, id('y))))

      evaluating(shared) should equal {
        lit("4") * (lit("1") + lit("1"))
      }
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
        choice('A) (
          'a -> dim('A)('b, 'c) {
            choice('A) (
               'b -> lit("3"),
               'c -> lit("7")
            )
          },
          'b -> lit("42")
        )}

      val selects1 = select('A,'b, select('A,'a, dimBody));
      val selects2 = select('A,'a, select('A,'b, dimBody));

      evaluating(selects1) should equal { lit("3") }

      // undeclared dimension
      evaluating(selects2)
      vacuousWarning should be (true)

    }


    // Examples from the paper
    it should "handle the examples from the vamos2013 paper correctly" in {

      // select D.a from
      //   dim D<a,b> in D<1,2>
      val example3_0 = select('D, 'a,
          dim('D)('a, 'b) { choice('D) (
            'a -> lit("1"),
            'b -> lit("2")
          )})

      evaluating(example3_0) should equal { lit("1") }

      // share v = e in
      //   dim X<y, z> in
      //     X<select A.b from v, select A.c from v>
      //
      // with e being something like: dim A<a,b,c> in A<1,2,3>
      val example3_1_e = dim('A)('a, 'b, 'c) { choice('A) (
        'a -> lit("1"),
        'b -> lit("2"),
        'c -> lit("3")
      )}

      val example3_1 = share('v, example3_1_e, dim('X)('y, 'z) { choice('X) (
        'y -> select('A, 'b, id('v)),
        'z -> select('A, 'c, id('v))
      )})

      val example3_1_sel1 = select('X, 'y, example3_1);
      val example3_1_sel2 = select('X, 'z, example3_1);

      evaluating(example3_1_sel1) should equal { lit("2") }
      evaluating(example3_1_sel2) should equal { lit("3") }

      // Section 4: Open Questions

      // 4.1 Undeclared Dimension
      // Design decision: Not allowed
      val example4_1 = select('D, 't, lit("1") + lit("2"))

      evaluating(example4_1)
      vacuousWarning should be (true)

      // 4.2 Multiple Dimensions
      // Design decision: Not allowed
      val example4_2 = select('D, 'a,
          dim('D)('a, 'b) { choice('D) (
              'a -> lit("1"),
              'b -> lit("2")
          )} + dim('D)('a, 'c) { choice('D) (
              'a -> lit("3"),
              'c -> lit("4")
          )})

      running {
        evaluating(example4_2)
      } should produce [FatalPhaseError] match {
        case error => error.phase should equal ('dimensionchecker)
      }

      // 4.3 Undeclared Tag
      // Design decision: Not allowed
      val example4_3 = select('D,'a, dim('D)('b, 'c) { choice('D) (
          'b -> lit("1"),
          'c -> lit("2")
        )})
      val example4_3_2 = select('D,'a, dim('D)('b, 'd) { lit("3") + lit("4") })

      running {
        evaluating(example4_3)
      } should produce [FatalPhaseError] match {
        case error => error.phase should equal ('dimensionchecker)
      }

      running {
        evaluating(example4_3_2)
      } should produce [FatalPhaseError] match {
        case error => error.phase should equal ('dimensionchecker)
      }

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
          choice('A) (
            'a -> dim('B)('c, 'd) { choice('B) (
              'c -> lit("1"),
              'd -> lit("2")
              )},
            'b -> lit("3")
          )})

      evaluating(example4_4)
      vacuousWarning should be (true)
    }
  }

  evaluator
}