package choicecalculus
package recovery

import lang.ASTNode
import lang.choicecalculus.{ Choices, Choice }
import lang.prettyprinter._

/**
 * @constructor
 *  
 * @param mapping from variable names to choice calculus expressions
 */
case class Solution(mapping: Map[Symbol, ASTNode]) extends Ordered[Solution] {

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

  private def collectDims(sol: ASTNode): Set[Symbol] = sol match {
    case Choices(dim, cases) => Set(dim) ++ cases.flatMap { 
      case Choice(_, c) => collectDims(c)
    }.toSet

    case _ => Set()
  }

  private def numberOfLeafs(sol: ASTNode): Int = sol match {
    case Choices(dim, cases) => cases.map { 
      case Choice(_, c) => numberOfLeafs(c)
    }.sum
    
    case _ => 1
  }

  override def toString: String = mapping map { 
    case (variable, tree) => s"${variable}:\n${tree.pretty}"
  } mkString "\n\n"
}