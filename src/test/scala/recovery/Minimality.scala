package choicecalculus
package recovery

import org.scalatest._
import org.scalatest.matchers.ShouldMatchers._

import utility.Table
import lang.ASTNode
import lang.javascript.AtomLit
import lang.choicecalculus.{ Choices, Choice }
class MinimalityTest extends FlatSpec {

  object min extends Minimality with CCRecovery

  import min.SolutionOrdering

  it should "generate the correct set of solution candidates" in {
    val sols1: Set[ASTNode] = min.allSolutionCandidates(('A,Set('a)) :: ('B, Set('a)) :: Nil, AtomLit("1") :: AtomLit("2") :: Nil)
    sols1.contains(AtomLit("1")) should equal (true)
    sols1.size should equal (8)
  }

  it should "generate the correct list of possible dims" in {
    min.allDimensions(2,3).size should equal (6)
  }

  implicit def int2atom(n: Int): AtomLit = AtomLit(n.toString)

  it should "find the minimal solution" in {
    
    var srcTable = new CloneInstanceTable('v_0, 'v_1) {
      | (1) | (1) |;
      | (2) | (1) |;
    }

    val minimalSolution = Map(
      'v_0 -> Choices('d_1, List(Choice[ASTNode]('t_1, 1), Choice[ASTNode]('t_2, 2))),
      'v_1 -> AtomLit("1")
    )

    // A<1,2> + A<1, B<1,2>>
    val sol = min.generateAllSolutions(srcTable)

    sol.contains(minimalSolution) should equal (true)
    SolutionOrdering.compare(min.minimalSolution(srcTable), minimalSolution) should equal (0)
  }

  it should "find the minimal solution2" in {
    
    var srcTable = new CloneInstanceTable('v_0, 'v_1) {
      | (1) | (1) |;
      | (2) | (2) |;
    }

    val minimalSolution = Map(
      'v_0 -> Choices('d_1, List(Choice[ASTNode]('t_1, 1), Choice[ASTNode]('t_2, 2))),
      'v_1 -> Choices('d_1, List(Choice[ASTNode]('t_1, 1), Choice[ASTNode]('t_2, 2)))
    )

    // A<1,2> + A<1, B<1,2>>
    val sol = min.generateAllSolutions(srcTable)

    sol.contains(minimalSolution) should equal (true)
    SolutionOrdering.compare(min.minimalSolution(srcTable), minimalSolution) should equal (0)
  }

  ignore should "find the minimal solution3" in {
    
    var srcTable = new CloneInstanceTable('v_0, 'v_1) {
      | (1) | (1) |;
      | (2) | (2) |;
      | (2) | (1) |;
    }

    println(min.minimalSolution(srcTable))
  }

}