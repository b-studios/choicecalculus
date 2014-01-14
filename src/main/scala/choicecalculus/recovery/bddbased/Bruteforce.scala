package choicecalculus
package recovery
package bddbased

import lang.ASTNode
import lang.choicecalculus.{ Choice, Alternative }

import scala.math.{ ceil, pow, log }

import utility.combinatorics._

/**
 * Bruteforce solver to recover choice calculus expressions from clone
 * instance tables.
 *
 * The recovery works as follows:
 * <ol>
 *   <li> generate all permutations of the table's rows [1]
 *   <li> for every permutation and every column of the permutation create a
 *        reduced OBDD [2]
 *   <li> return the smallest solution [3]
 * </ol>
 *
 * The solver relies on binary choices in order to correspond to BDDs.
 *
 * @example {{{
 *   object solver extends BruteforceSolver
 *   solver.globalSolution(new CloneInstanceTable('x, 'y) {
 *     | (1) | (1) |;
 *     | (2) | (1) |;
 *   })
 *   // => Map('x -> D1<1,2>, 'y -> 1)
 * }}}
 *
 *
 * <h2>Implementation Details
 *
 * Ad 1.: Not all permutations are generated, but the first row is fixed. Also see
 * `permutations` in [[utility.combinatorics]].
 *
 * Ad 2.: The reduced OBDD is created by using hash consing. Also see [[BDDBuilder]]
 *
 * Ad 3.: Minimality of solutions is defined in [[Solution]] via implementation of
 *        trait [[Ordered]]
 */
trait BruteforceSolver {

  /**
   * Finds the minimal choice calculus representation for a given clone instance table.
   *
   * <strong>Currently only works for tables with up to 8 rows</strong>
   *
   * Also see the trait's documentation for a short description of the algorithm.
   *
   * <h2>Notes
   *
   * Could easiliy be rewritten to return a "set" of minimal solutions
   *
   * If `table.rows.size` >= 9 we need a termination heuristic. For instance,
   * stop if there haven't been improvements in the last 10.000 permutations.
   *
   * Adding the 4th nested binary dimension leads to this combinatorial explosion...
   *
   * Another possible <em>approximation</em> would be to fix (n-7) but at least one row.
   *
   * @param table the clone instance table. Has to contain at least one row and one column
   *
   * @return the minimal solution expressable in binary choices
   *
   */
  def globalSolution(table: CloneInstanceTable): Solution = {

    require(table.rows.size > 0)
    require(table.columns.size > 0)

    var smallestSolution: Option[Solution] = None

    val rows = table.rows.toList
    val indices = table.columns.indices.toList

    def update(candidate: List[ASTNode]) {
      val sol: Solution = Solution((table.headers zip candidate).toMap)
      smallestSolution = smallestSolution map (sol min _) orElse Some(sol)
    }

    // The `foreach` could be parallelized, synchronizing around `update`
    rows permutations (nextPower(rows.size)) foreach { perm =>

      val solutionCandidate: List[ASTNode] = indices
        // project on variable
        .map { pi(perm, _) }
        // build tree per variable
        .map { bddBuilder build (_) getOrElse (sys error "Could not generate tree") }

      update(solutionCandidate)
    }

    smallestSolution getOrElse (sys error "Could not find a solution")
  }

  private def leaf(value: ASTNode): ASTNode = value

  /**
   * we limit ourselves to binary choices for now
   */
  private def binaryChoice(lvl: Int, lhs: ASTNode, rhs: ASTNode): ASTNode =
    Choice(Symbol(s"D$lvl"), Alternative('a, lhs) :: Alternative('b, rhs) :: Nil)

  // reuse definition from Minimality
  private def numberOfLeafs(sol: ASTNode): Int = sol match {
    case Choice(_, alts) => alts.map(numberOfLeafs).sum
    case _ => 1
  }

  private def bddBuilder = BDDBuilder.option[ASTNode, ASTNode, ASTNode](leaf, binaryChoice)

  private def nextPower(n: Int) = pow(2, ceil(log(n) / log(2))).toInt

  private def pi(rows: List[Option[List[ASTNode]]], idx: Int) = for {
    optRow <- rows
    projection = optRow map (r => r(idx))
  } yield projection
}