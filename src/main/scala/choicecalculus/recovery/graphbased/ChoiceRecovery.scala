package choicecalculus
package recovery
package graphbased

import lang.trees.{ Alternative, Choice, Tree }
import labeling.{ Path, Label }

private[recovery] trait ChoiceRecovery { self: Dimensions =>

  type Labeled[T] = Map[T, Label]

  /**
   * Returns a choice calculus expression, that represents all
   * labeled Trees provided as input.
   *
   */
  def toCC(labels: Labeled[Tree]): Option[Tree] = labels.size match {
    case 0 => sys error ""
    case 1 => Some(labels.head._1)
    case _ =>

      val cheapestDim = (for {
        (dim, tags) <- dimensions
        exp = expansion(dim, labels)
        if canChoose(dim, exp)
        cost = size(exp)
      } yield (dim, cost, exp)).toSeq.sortBy(_._2).headOption

      for {
        (dim, _, ls) <- cheapestDim
      } yield Choice(dim, splitNodes(ls, dim))
  }

  // split nodes into cases according to the labels
def splitNodes(labels: Labeled[Tree], dim: Symbol): List[Alternative] =
    (for {
      tag <- dimensions(dim)
      ls = for {
        (value, label) <- labels
        if label.contains((dim, tag))
        labelsWithout = label.removeChoicesFor(dim).clearEmptyPaths
      } yield (value, labelsWithout)

      ccexpr <- toCC(ls)
    } yield Alternative(tag, ccexpr)).toList

  // All tags must be present (Only call with expanded labels)
  def canChoose(dim: Symbol, labels: Labeled[Tree]): Boolean =
    dimensions(dim).forall { tag =>
      labels.exists {
        case (_, l) => l.contains((dim, tag))
      }
    }

  def expand(label: Label, dim: Symbol): Label = Label(label.paths.flatMap {
    case p if p.containsChoiceFor(dim) => Set(p)
    // expansion
    case p => dimensions(dim).map { tag => p :+ ((dim, tag)) }
  })

  def expansion(dim: Symbol, labels: Labeled[Tree]) = labels.map {
    case (value, label) => (value, expand(label, dim))
  }

  def size(labels: Labeled[Tree]): Int =
    labels.map { case (_, l) => l.size }.reduce(_ + _)

}
