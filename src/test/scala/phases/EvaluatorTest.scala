package choicecalculus
package phases
package evaluator

import org.scalatest._

import lang.ASTNode
import lang.JsCcParser

import org.kiama.attribution.Attribution.initTree

import utility.test
import utility.messages._

class EvaluatorTest extends FlatSpec with matchers.ShouldMatchers {

  import lang.javascript.{ Expression, Program, VarDeclStmt, Statement,
                           VarBinding, ReturnStmt }

  import lang.choicecalculus.{ Dimension }


  /**
   * this is called `evaluating` in scala test 1.9
   *
   * Can be removed after migrating to scala test 2.0
   */
  def running[T](block: => T) = evaluating(block)

  trait Context extends Evaluator with Reader with Parser with Namer
      with DimensionChecker with JsCcParser with test.Helpers 
      with namer.SymbolPreservingRewriter {

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
  }

  it should "find correct dimensioning" in new Context {

    // dim A<a> { A<a: 1> }
    val test_dim: Dimension[ASTNode] = dim('A)('a) { choice('A) ('a -> lit("1")) }
    // select A.a from dim A<a> { A<a: 1> }
    val test_selection = Program( select[Statement]('A, 'a, test_dim ) :: Nil)

    selecting(test_selection) should be {
      Program(lit("1") :: Nil)
    }

    val dimGraph = DependencyGraph(Set(DependentDimension(test_dim)))

    mute {
      dimensioning(test_dim) should be (dimGraph)
      dimensioning(test_selection).fullyConfigured should be (true)
    }
  }

  it should "not change plain hostlanguage programs" in new Context {
    substituting(lit("foo")) should be {
      lit("foo")
    }
    substituting(lit("3") + lit("5")) should be {
      lit("3") + lit("5")
    }
  }

  it should "substitute fully configured shares" in new Context {

    // share x = 3+5 in 4 * x
    val share_exp = share('x, lit("3") + lit("5"), lit("4") * id('x))

    substituting(share_exp) should be {
      lit("4") * (lit("3") + lit("5"))
    }

    // share x = { var foo = 4 } in x
    val share_stmt = share('x, VarDeclStmt(VarBinding(lit("foo"), lit("4")) :: Nil), id('x))

    substituting(share_stmt) should be {
      VarDeclStmt(VarBinding(lit("foo"), lit("4")) :: Nil)
    }

    mute {
      dimensioning(share_exp).fullyConfigured should be (true)
      dimensioning(share_exp.body).fullyConfigured should be (true)
    }
  }

  it should "first perform selections and then substitute shares" in new Context {


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

    evaluating(select('A, 'a, shareX( dimA ))) should be {
      ReturnStmt(Some(lit("3") + lit("5")))
    }

    evaluating(select('A, 'b, shareX( dimA ))) should be {
      ReturnStmt(Some(lit("2") * (lit("3") + lit("5"))))
    }
  }


  // TODO create own test section on includes
  it should "include files and calculate dimensions correctly" in new Context {

    // select Config.advanced from
    //   include "included.cclang"
    val stmt = select('Config, 'advanced, include("examples/include/included.js.cc", parsers.topLevel))

    evaluating(stmt) should be { Program(lit("14") :: Nil) }
  }

  // Move in separate testfile on 'substitution'
  it should "detect correctly whether a variable is used or not" in new Context {

    val expr = select('Config, 'advanced, id('foo))
    initTree(expr)
    variableIsUsed('foo)(expr) should be (true)
    variableIsUsed('bar)(expr) should be (false)

  }

  it should "reduce multiple nested shares correctly" in new Context {

    val share_exp  = share('x, lit("3") + lit("5"), share('y, id('x), id('x) * lit("5")))
    val share_exp2 = share('x, lit("3"), share('y, id('x) * lit("5"), id('y)))
    val share_exp3 = share('x, lit("3"), share('y, lit("5") * id('x), id('y)))

    substituting(share_exp) should be {
      (lit("3") + lit("5")) * lit("5")
    }

    substituting(share_exp2) should be { lit("3") * lit("5") }
    substituting(share_exp3) should be { lit("5") * lit("3") }

    mute {
      dimensioning(share_exp).fullyConfigured should be (true)
    }
  }

  it should "perform selections on shared dimensions" in new Context {

    val test_dim = dim('A)('a) { choice('A) ('a -> lit("1")) }
    val exp = share('x, test_dim, select('A, 'a, id('x)))

    selecting(exp) should be {
      share('x, test_dim, partialConfig('A -> 'a) (id('x)))
    }
    evaluating(exp) should be { lit("1") }

    mute {
      dimensioning(exp).fullyConfigured should be (true)
    }
  }

  it should "perform selections in shared dependend dimensions" in new Context {

    val inner_dim = dim('A)('a) { choice('A) ('a -> lit("1")) }
    val outer_dim = dim('B)('b) { choice('B) ('b -> inner_dim) }
    val shared = share('x, outer_dim, select('A, 'a, select('B, 'b, id('x))))

    evaluating(shared) should be { lit("1") }
  }

  // share x = dim A<a> { A<1> + A<1> } in
  //   share y = dim B<b> { 4 * B<select A.a from x> } in
  //     select B.b from y
  it should "perform selections on renamed dimensions" in new Context {

    val first_dim = dim('A)('a) {
      choice('A) ('a -> lit("1")) + choice('A) ('a -> lit("1"))
    }

    val second_dim = dim('B)('b) {
      lit("4") * choice('B) ('b -> select('A, 'a, id('x)))
    }

    val shared = share('x, first_dim, share('y, second_dim, select('B, 'b, id('y))))

    evaluating(shared) should be {
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
  it should "perform inner selections first" in new Context {

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

    evaluating(selects1) should be { lit("3") }

    // undeclared dimension
    evaluating(selects2)
    vacuousWarning should be (true)

  }


  it should "correctly substitute shares with free choices (#7)" in new Context {

    val ticket7_1 = select('A, 'a, dim('A)('a,'b) {
      share('v, choice('A)('a -> lit("42"), 'b -> lit("43")),
        id('v))
    })

    val ticket7_2 = select('A, 'a, select('B, 'c, 
      dim('A)('a,'b) {
        share('v, dim('B)('c, 'd) {
          choice('B)(
            'c -> choice('A)('a -> lit("42"), 'b -> lit("43")),
            'd -> choice('A)('a -> lit("44"), 'b -> lit("45"))
          )},
          id('v)
        )
      }))

    evaluating(ticket7_1) should be { lit("42") }
    evaluating(ticket7_2) should be { lit("42") }
  }

  "Examples from vamos 2013" should
    "3.0 select simple dimensions" in new Context {

    // select D.a from
    //   dim D<a,b> in D<1,2>
    val example3_0 = select('D, 'a,
      dim('D)('a, 'b) { choice('D) (
        'a -> lit("1"),
        'b -> lit("2")
      )})

    evaluating(example3_0) should be { lit("1") }
  }

  it should "3.1 select from shared variational expressions" in new Context {

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

    evaluating(example3_1_sel1) should be { lit("2") }
    evaluating(example3_1_sel2) should be { lit("3") }
  }
}