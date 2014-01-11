package choicecalculus
package phases
package dimensionchecker

import org.scalatest._

import lang.ASTNode
import lang.JsCcParser

import utility.test
import utility.messages._

class DimensionCheckerTest extends FlatSpec with matchers.ShouldMatchers {

  import lang.javascript.BlockStmt

  def plain: Set[GraphNode] = Set.empty

  def dimensionCheckerError[T](block: => T): FatalPhaseError =
    evaluating {
      block
    } should produce [FatalPhaseError] match {
      case error => {
        error.phase should equal ('dimensionchecker)
        error
      }
    }

  trait Context extends Reader with Parser with Namer with DimensionChecker
    with JsCcParser with test.Helpers {

    def dimensionChecking(tree: ASTNode): ASTNode = {
      resetMessages()
      runReader(tree)
      runNamer(tree)
      runDimensionChecker(tree)
    }

  }

  it should "merge the choices of multiple subtrees correclty" in new Context {

    val ast = dim('A)('a,'b) {
      BlockStmt(List(
        choice('A)('a -> lit("1"), 'b -> lit("2")),
        choice('A)('a -> lit("3"), 'b -> lit("4")),
        choice('A)('a -> lit("5"), 'b -> lit("6"))
      ))
    }

    val dimGraph = DimensionGraph(Set(
      DimensionNode('A, 'a :: 'b :: Nil, Map('a -> plain, 'b -> plain))
    ))

    dimensionChecking(ast)->dimensioning should equal (dimGraph)

  }

  "Open questions from vamos 2013" should
    "4.1 warn when selecting undeclared dimensions" in new Context {

    val example4_1 = select('D, 't, lit("1") + lit("2"))
    dimensionChecking(example4_1)
    vacuousWarning should be (true)
  }

  it should "4.2 not allow multiple, parallel dimension declarations" in new Context {
    val example4_2 = select('D, 'a,
        dim('D)('a, 'b) { choice('D) (
            'a -> lit("1"),
            'b -> lit("2")
        )} + dim('D)('a, 'c) { choice('D) (
            'a -> lit("3"),
            'c -> lit("4")
        )})

    dimensionCheckerError { dimensionChecking(example4_2) }
  }

  it should "4.3 not allow undeclared tag selection" in new Context {
    val example4_3 = select('D,'a, dim('D)('b, 'c) { choice('D) (
        'b -> lit("1"),
        'c -> lit("2")
      )})
    val example4_3_2 = select('D,'a, dim('D)('b, 'd) { lit("3") + lit("4") })

    dimensionCheckerError { dimensionChecking(example4_3) }
    dimensionCheckerError { dimensionChecking(example4_3_2) }
  }

  it should "4.4 warn when selecting into dependent dimensions" in new Context {

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

    dimensionChecking(example4_4)
    vacuousWarning should be (true)
  }
}