package choicecalculus
package recovery

import lang.trees.{ Alternative, Choice, Tree }
import lang.jscc.PrettyPrinter._

/**
 * @constructor
 *
 * @param mapping from variable names to choice calculus expressions
 */
case class Solution(mapping: Map[Symbol, Tree]) extends Ordered[Solution] {

  /**
   * Compares the two solutions by their size
   *
   * The ordering is expressed in terms of <strong>number of leafs</strong
   * and <strong>number of used dimensions</strong>.
   *
   * @param other the other solution to compare with
   * @return 0 if both solutions are equally sized. Negative value
   *         if other > this. Positive value if this > other.
   *
   * @see [[Ordered]]
   */
  def compare(other: Solution) =
    (numberOfLeafs compare other.numberOfLeafs) match {
      case 0 => numberOfDims compare other.numberOfDims
      case n => n
    }

  /**
   * returns the smaller solution
   */
  def min(other: Solution) = if (other > this) other else this

  /**
   * Computes the number of dimensions as referenced by choices within `mapping`
   */
  def numberOfDims: Int = mapping.flatMap {
    case (_, c) => collectDims(c)
  }.toSet.size

  /**
   * Computes the number of leafs contained in the ast nodes of `mapping`
   */
  def numberOfLeafs: Int = mapping.map {
    case (_, c) => numberOfLeafs(c)
  }.sum

  private def collectDims(sol: Tree): Set[Symbol] = sol match {
    case Choice(dim, alts) => Set(dim) ++ alts.flatMap {
      case Alternative(_, c) => collectDims(c)
    }.toSet

    case _ => Set()
  }

  private def numberOfLeafs(sol: Tree): Int = sol match {
    case Choice(dim, alts) => alts.map {
      case Alternative(_, c) => numberOfLeafs(c)
    }.sum

    case _ => 1
  }

  override def toString: String = mapping map {
    case (variable, tree) => s"${variable}:\n${tree.pretty}"
  } mkString "\n\n"
}