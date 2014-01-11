package choicecalculus
package recovery
package bddbased

import org.scalatest._
import org.scalatest.matchers.ShouldMatchers._

import lang.ASTNode
import lang.javascript.AtomLit

// TODO add expected result to actually test this
class BruteforceTest extends FlatSpec with utility.test.Helpers with BruteforceSolver {

  globalSolution(new CloneInstanceTable('x, 'y, 'z) {
    | (1) | (4) | (1) |;
    | (1) | (4) | (2) |;
    | (2) | (5) | (1) |;
    | (2) | (5) | (3) |;
    | (3) | (4) | (1) |;
    | (3) | (4) | (2) |;
    | (3) | (5) | (1) |;
    | (3) | (5) | (3) |;
  })

  // A<B<1,2>,3> + B<4,5> + C<1, B<2,3>>
  globalSolution(new CloneInstanceTable('x, 'y, 'z) {
    | (1) | (4) | (1) |;
    | (1) | (4) | (2) |;
    | (2) | (5) | (1) |;
    | (2) | (5) | (3) |;
    | (3) | (4) | (1) |;
    | (3) | (4) | (2) |;
    | (3) | (5) | (1) |;
    | (3) | (5) | (3) |;
  })

  // Cai's previous counter example
  globalSolution(new CloneInstanceTable('w, 'x, 'y, 'z) {
    | (1) | (1) | (1) | (1) |;
    | (2) | (1) | (1) | (2) |;
    | (2) | (2) | (2) | (3) |;
    | (2) | (3) | (3) | (3) |;
  })

}