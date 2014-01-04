package choicecalculus
package recovery

import lang.ASTNode
import lang.choicecalculus.{ Choices, Choice }

package object solution {

  type Solution = Map[
    Symbol, // Variable name
    ASTNode // Choice calculus expression
  ]

  implicit object SolutionOrdering extends Ordering[Solution] {
    def compare(a: Solution, b: Solution) = 
      (numberOfLeafs(a) compare numberOfLeafs(b)) match {
        case 0 => numberOfDims(a) compare numberOfDims(b)
        case n => n      
      }
  }

  private def numberOfLeafs(sol: Solution): Int = 
    sol.map { case (_, c) => numberOfLeafs(c) }.sum
    
  private def numberOfLeafs(sol: ASTNode): Int = sol match {
    case Choices(dim, cases) => cases.map { 
      case Choice(_, c) => numberOfLeafs(c)
    }.sum
    
    case _ => 1
  }
  
  private def numberOfDims(sol: Solution): Int = 
    sol.flatMap { 
      case (_, c) => collectDims(c) 
    }.toSet.size
    
  private def collectDims(sol: ASTNode): Set[Symbol] = sol match {
    case Choices(dim, cases) => Set(dim) ++ cases.flatMap { 
      case Choice(_, c) => collectDims(c)
    }.toSet

    case _ => Set()
  }
}