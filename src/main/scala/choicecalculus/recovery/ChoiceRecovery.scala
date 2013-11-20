package choicecalculus
package recovery

private[recovery] trait ChoiceRecovery[T] { self: Dimensions with PathLabels with Choices[T] =>
  
  // has to be invoked once per variable    
  def toCC(labels: Map[T, Label]): Option[CCExpr] = labels.size match {
    case 0 => sys error ""
    case 1 => Some(Literal(labels.head._1))
    case _ => 

    val cheapestDim = (for {
      (dim, tags) <- dimensions
      exp = expansion(dim, labels)
      if canChoose(dim, exp)
      cost = size(exp)      
    } yield (dim, cost, exp)).toSeq.sortBy(_._2).headOption
    
    for {
      (dim, _, ls) <- cheapestDim
    } yield CCChoice(dim, splitNodes(ls, dim))
  }
  
  // split nodes into cases according to the labels
  def splitNodes(labels: Map[T, Label], dim: Symbol): List[CCCase] =
    (for {
      tag <- dimensions(dim)
      ls = for {
        (value, label) <- labels
        if label.contains((dim, tag))
        labelsWithout = label.removeChoicesFor(dim).clearEmptyPaths       
      } yield (value, labelsWithout)
      
      ccexpr <- toCC(ls)
    } yield CCCase(tag, ccexpr)).toList
  
   
  // All tags must be present (Only call with expanded labels)
  def canChoose(dim: Symbol, labels: Map[T, Label]): Boolean = 
    dimensions(dim).forall { tag => 
      labels.exists { 
        case (_, l) => l.contains((dim, tag)) 
      }      
    }
    
  def expansion(dim: Symbol, labels: Map[T, Label]) = labels.map {
    case (value, label) => (value, label.expand(dim)) 
  }
  
  def size(labels: Map[T, Label]): Int = 
    labels.map{ case (_, l) => l.size }.reduce(_+_)
}
