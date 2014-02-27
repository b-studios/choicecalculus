package choicecalculus
package phases
package evaluator

import org.scalatest._

import lang.trees.{ Dimension, Tree }
import lang.javascript.trees.{ Expression, Program, VarDeclStmt, Statement,
                               VarBinding, ReturnStmt }

import lang.jscc.JsCcParser

import org.kiama.attribution.Attribution.initTree

import utility.test
import utility.messages._

class EvaluatorTest extends FlatSpec with matchers.ShouldMatchers {

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
    def selecting(tree: Tree): Tree = {
      resetMessages()
      runReader(tree)
      runNamer(tree)
      runDimensionChecker(tree)
      rewrite(reduce(select))(tree)
    }

    def substituting(tree: Tree): Tree = {
      resetMessages()
      runReader(tree)
      runNamer(tree)
      runDimensionChecker(tree)
      rewrite(reduce(substitute + removeShares))(tree)
    }

    def evaluating(tree: Tree): Tree = {
      resetMessages()
      runReader(tree)
      runNamer(tree)
      runDimensionChecker(tree)
      runEvaluator(tree)
    }
  }

  it should "find correct dimensioning" in new Context {

    // dim A<a> { A<a: 1> }
    val test_dim: Dimension = dim('A)('a) { choice('A) ('a -> lit("1")) }
    // select A.a from dim A<a> { A<a: 1> }
    val test_selection = Program( select('A, 'a, test_dim ) :: Nil)

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
    def share_stmt = share('x, VarDeclStmt(VarBinding(lit("foo"), lit("4")) :: Nil), id('x))

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
    def shareX(body: Tree): Tree =
      share('x, lit("3") + lit("5"), body)

    // dim A<a, b> {
    //  choice A {
    //    case a => return x
    //    case b => return 2 * x
    //  }
    // }
    def dimA = dim('A)('a, 'b) {
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
    def stmt = select('Config, 'advanced, include("examples/include/included.js.cc", parsers.topLevel))

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
    def share_exp2 = share('x, lit("3"), share('y, id('x) * lit("5"), id('y)))
    def share_exp3 = share('x, lit("3"), share('y, lit("5") * id('x), id('y)))

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

    def test_dim = dim('A)('a) { choice('A) ('a -> lit("1")) }
    def exp = share('x, test_dim, select('A, 'a, id('x)))

    val exp1 = exp

    selecting(exp) should be {
      share('x, test_dim, partialConfig('A -> 'a) (id('x)))
    }
    evaluating(exp1) should be { lit("1") }

    mute {
      dimensioning(exp1).fullyConfigured should be (true)
    }
  }

  it should "perform selections in shared dependend dimensions" in new Context {

    def inner_dim = dim('A)('a) { choice('A) ('a -> lit("1")) }
    def outer_dim = dim('B)('b) { choice('B) ('b -> inner_dim) }
    def shared = share('x, outer_dim, select('A, 'a, select('B, 'b, id('x))))

    evaluating(shared) should be { lit("1") }
  }

  // share x = dim A<a> { A<1> + A<1> } in
  //   share y = dim B<b> { 4 * B<select A.a from x> } in
  //     select B.b from y
  it should "perform selections on renamed dimensions" in new Context {

    def first_dim = dim('A)('a) {
      choice('A) ('a -> lit("1")) + choice('A) ('a -> lit("1"))
    }

    def second_dim = dim('B)('b) {
      lit("4") * choice('B) ('b -> select('A, 'a, id('x)))
    }

    def shared = share('x, first_dim, share('y, second_dim, select('B, 'b, id('y))))

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

    def dimBody = dim('A)('a, 'b) {
      choice('A) (
        'a -> dim('A)('b, 'c) {
          choice('A) (
             'b -> lit("3"),
             'c -> lit("7")
          )
        },
        'b -> lit("42")
      )}

    def selects1 = select('A,'b, select('A,'a, dimBody));
    def selects2 = select('A,'a, select('A,'b, dimBody));

    evaluating(selects1) should be { lit("3") }

    // undeclared dimension
    evaluating(selects2)
    vacuousWarning should be (true)

  }


  it should "correctly substitute shares with free choices (#7)" in new Context {

    def ticket7_1 = select('A, 'a, dim('A)('a,'b) {
      share('v, choice('A)('a -> lit("42"), 'b -> lit("43")),
        id('v))
    })

    def ticket7_2 = select('A, 'a, select('B, 'c, 
      dim('A)('a,'b) {
        share('v, dim('B)('c, 'd) {
          choice('B)(
            'c -> choice('A)('a -> lit("42"), 'b -> lit("43")),
            'd -> choice('A)('a -> lit("44"), 'b -> lit("45"))
          )},
          id('v)
        )
      }))

    // select A.a from (dim A<a,b> in share #v = A<a: 1, b: 2> in dim B<c,d> in #v)
    def ticket7_3 = select('A, 'a, dim('A)('a, 'b) {
        share('v, choice('A)('a -> lit("1"), 'b -> lit("2")), dim('B)('c, 'd) {
          id('v)
        })
      })

    // select B.c from select A.a from dim A<a,b> in dim B<c,d> in share #v = A<a: 1, b: 2> in share #w = B<c: #v, d: #v> in #w + #w
    def ticket7_4_body = dim('A)('a, 'b) { dim('B)('c, 'd) { 
        share('v, choice('A)('a -> lit("1"), 'b -> lit("2")), 
          share('w, choice('B)('c -> id('v), 'd -> id('v)), id('w)))
      }}

    def ticket7_4_1 = select('A, 'a, select('B, 'd, ticket7_4_body))
    def ticket7_4_2 = select('B, 'c, select('A, 'b, ticket7_4_body))

    evaluating(ticket7_1) should be { lit("42") }
    evaluating(ticket7_2) should be { lit("42") }
    evaluating(ticket7_3) should be { dim('B)('c, 'd) { lit("1") } }
    evaluating(ticket7_4_1) should be { lit("1") }
    evaluating(ticket7_4_2) should be { lit("2") }
  }

  it should "not substitute shares with unselected free choices" in new Context {
    // dim A<a, b> in share #v = A <a : 1, b : 2> in #v
    def ex = dim('A)('a, 'b) {
        share('v, choice('A)('a -> lit("1"), 'b -> lit("2")), id('v))
      }

    evaluating(ex) should be { ex }
  }

  it should "select recursively nested choices (dominant choices)" in new Context {
    // select A.a from dim A<a,b> in A<a:A<a:1,b:2>,b:3>
    def ast = dim('A)('a,'b) {
        choice('A)('a -> choice('A)('a -> lit("1"), 'b -> lit("2")), 'b -> lit("3"))
      }

    evaluating(select('A, 'a, ast)) should be { lit("1") }
    evaluating(select('A, 'b, ast)) should be { lit("3") }
  }

  // select A.a from select B.a from 
  //  dim A<a,b> in (dim B<a,b> in 3) + A<a: (dim B<c,d> in 4), b: 5>
  it should "force selection to be performed inside out #9" in new Context {
    def ast = dim('A)('a, 'b) {
      (dim('B)('a, 'b) { lit("3") }) + choice('A)('a -> (dim('B)('a, 'b) { lit("4") }), 'b -> lit("5"))
    }

    evaluating(select('B, 'a, ast)) should be { dim('A)('a, 'b) { lit("3") + choice('A)('a -> (dim('B)('a, 'b) { lit("4") }), 'b -> lit("5")) }}
    evaluating(select('A, 'b, select('B, 'a, ast))) should be { lit("3") + lit("5") }
    evaluating(select('A, 'a, select('B, 'a, ast))) should be { lit("3") + (dim('B)('a, 'b) { lit("4") }) }
  }

  "Examples from the project report" should
    "support reuse of variation points" in new Context {
      def ast = dim('Par)('x, 'y, 'z) {
          dim('Impl)('plus, 'times) {
            share('v, choice('Par)('x -> lit("x"), 'y -> lit("y"), 'z -> lit("z")), 
              choice('Impl)('plus -> (id('v) + id('v)), 'times -> (lit("2") * id('v))))
          }
        }

      evaluating(select('Par, 'x, ast)) should be { 
        dim('Impl)('plus, 'times) {
          choice('Impl)('plus -> (lit("x") + lit("x")), 'times -> (lit("2") * lit("x")))
        }
      }
  }

  it should "support reuse of alternatives" in new Context {
    def ast = dim('OS)('linux, 'mac, 'windows) {
      share('v, lit("\n"), choice('OS)(
        'linux -> id('v),
        'mac -> id('v),
        'windows -> lit("\r\n")
      ))
    }
    evaluating(select('OS, 'mac, ast)) should be { lit("\n") }
    evaluating(select('OS, 'windows, ast)) should be { lit("\r\n") }
  }

  it should "support reuse of common subexpressions" in new Context {
    def ast = dim('Secure)('no, 'yes) {
      share('read, lit("readInput"), choice('Secure)(
        'no  -> id('read),
        'yes -> (lit("secure") + id('read) + lit("secureEnd"))
      ))
    }
    evaluating(ast) should be { 
      dim('Secure)('no, 'yes) { choice('Secure)(
          'no  -> lit("readInput"),
          'yes -> (lit("secure") +  lit("readInput") + lit("secureEnd"))
        )
      }
    }
    evaluating(select('Secure, 'yes, ast)) should be { lit("secure") +  lit("readInput") + lit("secureEnd") }
    evaluating(select('Secure, 'no, ast)) should be { lit("readInput") }
  }

  it should "support reuse of variational components" in new Context {
    def ast = share('sort, dim('SortAlg)('merge, 'quick) {
      choice('SortAlg)(
        'quick -> lit("quickSort"),
        'merge -> lit("mergeSort")
      )
    }, select('SortAlg, 'merge, id('sort)) + select('SortAlg, 'quick, id('sort)))

    evaluating(ast) should be { lit("mergeSort") + lit("quickSort")}
  }

  it should "support reuse of atomic choices" in new Context {
    def astV = share('v, dim('Par)('x,'y, 'z) { choice('Par)(
      'x -> lit("x"),
      'y -> lit("y"),
      'z -> lit("z")
    )}, id('v) + id('v))

    def astW = share('w, dim('Par)('a,'b) { choice('Par)(
        'a -> lit("a"),
        'b -> lit("b")
      )}, id('w) + id('w) + id('w))

    evaluating(select('Par, 'y, astV)) should be { lit("y") + lit("y") }
    evaluating(select('Par, 'a, astW)) should be { lit("a") + lit("a") + lit("a") }
    
    evaluating(select('Par, 'y, astV) * astW) should be { 
      (lit("y") + lit("y")) * astW
    }

    evaluating(astV * select('Par, 'a, astW)) should be { 
      astV * (lit("a") + lit("a") + lit("a"))
    }

    evaluating(select('Par, 'y, astV) * select('Par, 'a, astW)) should be { 
      (lit("y") + lit("y")) * (lit("a") + lit("a") + lit("a"))
    }
  }

  it should "override default selections" in new Context {
    def ast = share('w, dim('Par)('a,'b) { choice('Par)(
        'a -> lit("a"),
        'b -> lit("b")
      )}, id('w) + id('w) + select('Par, 'b, id('w)))

    evaluating(select('Par, 'a, ast)) should be { lit("a") + lit("a") + lit("b") }
  }


  "Examples from vamos 2013" should
    "3.0 select simple dimensions" in new Context {

    // select D.a from
    //   dim D<a,b> in D<1,2>
    def example3_0 = select('D, 'a,
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
    def example3_1_e = dim('A)('a, 'b, 'c) { choice('A) (
      'a -> lit("1"),
      'b -> lit("2"),
      'c -> lit("3")
    )}

    def example3_1 = share('v, example3_1_e, dim('X)('y, 'z) { choice('X) (
      'y -> select('A, 'b, id('v)),
      'z -> select('A, 'c, id('v))
    )})

    def example3_1_sel1 = select('X, 'y, example3_1);
    def example3_1_sel2 = select('X, 'z, example3_1);

    evaluating(example3_1_sel1) should be { lit("2") }
    evaluating(example3_1_sel2) should be { lit("3") }
  }
}