package choicecalculus
package recovery

import lang.ASTNode

import org.kiama.rewriting.Rewriter.{ attempt, everywherebu, rewrite, rule }
import org.kiama.rewriting.Strategy

// we limit ourselves to binary choices first
case class BinaryChoice(dim: Symbol, lhs: ASTNode, rhs: ASTNode) extends ASTNode

// this is defined on a small subset of cc expressions. If necessary it needs to be
// instantiated to the complete object language and all additional CC extensions
//
// there are two approaches to rewriting. One is semantics driven (mostly that's what we
// do before we get the rules) and one is syntactical.
trait CNF {

  // TODO define some ordering on dimensions

  val ord: Ordering[Symbol]

  def minimize(node: ASTNode): ASTNode = rewrite(attempt(everywherebu(sort)) <* everywherebu(CCIdempLR))(node)

  lazy val sort: Strategy = CCSwapLR <+ (CCIdempRL <* CCSwapLR)

  lazy val CCIdempLR: Strategy = rule {
    case BinaryChoice(_, lhs, rhs) if lhs == rhs => lhs
  }
  
  lazy val CCSwapLR: Strategy = rule {
    case BinaryChoice(dim1,
      BinaryChoice(dim2, e1, e2), BinaryChoice(dim3, e3, e4)) if ord.equiv(dim2, dim3) =>
        BinaryChoice(dim2, BinaryChoice(dim1, e1, e3), BinaryChoice(dim1, e2, e4))
  }
  
  lazy val CCIdempRL: Strategy = rule {
    // A<C<1,2>, B<3,4>> --> A<B<C<1,2>, C<1,2>>, B<3,4>>
    case BinaryChoice(dim1,
      l@BinaryChoice(dim2, _, _), r@BinaryChoice(dim3, _, _)) 
        if ord.gt(dim2, dim3) => BinaryChoice(dim1, BinaryChoice(dim3, l, l), r)
        
    // A<B<1,2>,C<3,4>> --> A<B<1,2>, B<C<3,4>,C<3,4>>>
    case BinaryChoice(dim1,
      l@BinaryChoice(dim2, _, _), r@BinaryChoice(dim3, _, _)) 
        if ord.lt(dim2, dim3) => BinaryChoice(dim1, l, BinaryChoice(dim2, r, r))

    // A<1, B<3,4>> --> A<B<1,1>, B<3,4>>
    case BinaryChoice(dim1, l, r@BinaryChoice(dim2, _, _)) 
      if !l.isInstanceOf[BinaryChoice] && ord.lt(dim2, dim1) => BinaryChoice(dim1, BinaryChoice(dim2, l, l), r)

    // A<B<1,2>, 3> --> A<B<1,2>, B<3,3>>
    case BinaryChoice(dim1, l@BinaryChoice(dim2, _, _), r) 
      if !r.isInstanceOf[BinaryChoice] && ord.lt(dim2, dim1) => BinaryChoice(dim1, l, BinaryChoice(dim2, r, r))
  }
}

object CNF {
  def apply(dims: List[Symbol]): CNF = new CNF {
    val ord = new Ordering[Symbol] {
      def compare(a: Symbol, b: Symbol) = dims.indexOf(a) compare dims.indexOf(b)
    }
  }
}

/** This is based on recovery/Minimality.scala */
trait BruteforceSolver {

  import utility.setops._

  type Solution = Map[
    Symbol, // Variable name
    ASTNode // Choice calculus expression
  ]

  implicit object LocalSolutionOrdering extends Ordering[ASTNode] {
    def compare(a: ASTNode, b: ASTNode) = 
      (numberOfLeafs(a) compare numberOfLeafs(b)) match {
        case 0 => numberOfDims(a) compare numberOfDims(b)
        case n => n      
      }
  }

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
    case BinaryChoice(dim, lhs, rhs) => numberOfLeafs(lhs) + numberOfLeafs(rhs)    
    case _ => 1
  } 
  
  private def numberOfDims(sol: Solution): Int = 
    sol.flatMap { 
      case (_, c) => collectDims(c) 
    }.toSet.size

  private def numberOfDims(sol: ASTNode): Int = 
    collectDims(sol).size

  private[recovery] def collectDims(sol: ASTNode): Set[Symbol] = sol match {
    case BinaryChoice(dim, lhs, rhs) => Set(dim) ++ collectDims(lhs) ++ collectDims(rhs)
    case _ => Set.empty
  }

  // Partitioning
  // ------------
  // Partitioning has to respect already disjoint instances, otherwise it would always generate
  // permutations of the trivial solution which cannot be optimized locally.

  // partitioning has to be performed on all permutations of column ordering.

  trait Partitioning[+T]
  case class Elem[+T](v: T) extends Partitioning[T]
  case class Partition[+T](lhs: Partitioning[T], rhs: Partitioning[T]) extends Partitioning[T]


  // with 7 distinct rows thats already 665280
  def allPartitions[T](rows: Set[T]): Set[Partitioning[T]] = rows match {
    case _ if rows.isEmpty => Set.empty
    case _ if rows.size == 1 => Set(Elem(rows.head))
    case _ => {
      // this might not be the version with best performance...
      val allSubsets: Set[Set[T]] = rows.subsets.toSet -- Set(rows, Set.empty)
      for {
        set <- allSubsets
        p1 <- allPartitions(set)
        p2 <- allPartitions(rows -- set)
        part <- Set(Partition(p1, p2), Partition(p2, p1))
      } yield part
    }
  }

  // The actual solver
  def localSolution(node: ASTNode): ASTNode = collectDims(node).toList.permutations.map { dims => 
      CNF(dims).minimize(node) 
    }.min

  private def restoreChoice(part: Partitioning[List[ASTNode]], col: Int, level: Int = 0): ASTNode = part match {
    case Elem(row) => row(col)
    case Partition(lhs, rhs) => BinaryChoice(Symbol(s"D$level"),
        restoreChoice(lhs, col, level + 1), restoreChoice(rhs, col, level + 1))
  }

  def globalSolution(table: CloneInstanceTable): Solution =
    (for {
      partition <- allPartitions(table.rows)
      solution = table.headers.zipWithIndex.map {
        case (head, idx) => (head -> localSolution(restoreChoice(partition, idx)))
      }.toMap
    } yield solution).min

}