package choicecalculus
package recovery
package rewritingbased

import org.scalatest._
import org.scalatest.matchers.ShouldMatchers._

import org.kiama.rewriting.Strategy;

class BruteforceTest extends FlatSpec with utility.test.Helpers with BruteforceSolver {

  val cnf = CNF(List('A,'B,'C))

  // Fixtures:

  // A<1, B<2,3>>
  val ex1 = BinaryChoice('A, 1, BinaryChoice('B, 2, 3))

  // B<1, A<2,3>>
  val ex2 = BinaryChoice('B, 1, BinaryChoice('A, 2, 3))
  val ex2reordered = BinaryChoice('A, BinaryChoice('B, 1, 2), BinaryChoice('B, 1, 3))

  // A<B<C<1,2>, C<1,2>>, C<1,2>>
  val ex3 = BinaryChoice('A, BinaryChoice('B, BinaryChoice('C, 1, 2), BinaryChoice('C, 1, 2)), BinaryChoice('C, 1, 2))
  val ex3min = BinaryChoice('C, 1, 2)

  // A<C<B<1,1>, B<2,2>>, C<1,2>>
  val ex4 = BinaryChoice('A, BinaryChoice('B, BinaryChoice('C, 1, 2), BinaryChoice('C, 1, 2)), BinaryChoice('C, 1, 2))

  // Tests:

  it should "not rewrite terms already in correct order (and minimal)" in {
    cnf.minimize(ex1) should equal (ex1)
  }

  it should "rewrite a term to be in correct order" in {
    cnf.minimize(ex2) should equal (ex2reordered)
  }

  it should "rewrite to the locally minimal solution" in {
    localSolution(ex2reordered) should equal (ex2)
    localSolution(ex3) should equal (ex3min)
    localSolution(ex4) should equal (ex3min)
  }

  // scales pretty well in the number of variables, but not in number of variants
  // println(globalSolution(new CloneInstanceTable('w, 'x, 'y, 'z) {
  //   | (1) | (1) | (1) | (1) |;
  //   | (2) | (1) | (1) | (2) |;
  //   | (2) | (2) | (2) | (3) |;
  //   | (2) | (3) | (3) | (3) |;
  //   | (2) | (3) | (3) | (1) |;
  // }))

}