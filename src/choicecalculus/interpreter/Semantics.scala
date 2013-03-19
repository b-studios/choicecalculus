package choicecalculus
package semantics


// This is something like a "typechecker" only for dimensions

// Requirements for the dimension checker:
// 1. every occurence of a choice is bound by a proper dimension declaration
// 2. a set of dimensions can be assigned to every piece of code
/**
   * Important terminology:
   * 1. Well Formed: The expression does not contain unbound choices (and all choices have correct number of alternatives)
   * 2. Fully Configured: All choices have been resolved by selections.
   */
trait DimensionChecker { // self: ChoiceGraph =>
  
  import ast._
  import org.kiama.attribution.Attribution._
  import org.kiama.rewriting.Rewriter.{rule, sometd, reduce}
   
  /**
   * Due to wellformedness every choice has to be lexically nested inside of a dimension declaration
   * So we first remove that dimension declaration.
   */
  def selectFromDimension(dim: Symbol, tag: Symbol) = rule {
    case DimensionExpr(d, tags, body) => {
      // it could also be checked whether tag occurs in `tags`
      sometd( selectFromChoices(dim, tag) )(body) match {
        case Some(result) => result
        case _ => // maybe throw error here
      }
    }
  }
  def selectFromChoices(dim: Symbol, tag: Symbol) = rule {
    
    case ChoiceExpr(d, choices) if d == dim => {
      val matches = choices.filter {
        case Choice(t, body) => t == tag
      }
      
      if (matches.size == 0)
        sys error "Cannot select from choice because no tag matched"
        
      if (matches.size > 1)
        sys error "Cannot select from choice because multiple tags matched"
        
      matches(0).body
    }
  }
  
  /**
   * Maybe `reduce` isn't the right semantics, since inside of a dimension no other dimension-declaration
   * with the same name should be reduced.
   */
  def selectReduction = rule {
    // We apply selectFromDimension to every branch until it works (at any level)
    case SelectExpr(dim, tag, body) => sometd( selectFromDimension(dim, tag) )(body) match {
      case Some(result) => result
      case _ => None // maybe throw error here
    }
  }
  
  def eval = reduce ( selectReduction )
  /*
  case class Dimension(name: Symbol, tags: Set[Symbol]) {
    def isWildcard = tags.contains('_)
    override def toString = "%s<%s>".format(name.name, tags.map(_.name) mkString ", ")
  }
  object WildcardDim {
    def unapply(dim: Dimension): Option[(Symbol, Set[Symbol])] = 
      if (dim.isWildcard)
        Some( (dim.name, dim.tags) )
      else
        None
  }
 
  val env: ASTNode => List[DimensionExpr] = attr {
    case p if p.isRoot => List ()
    case n => (n.parent) match {
      case p@DimensionExpr(name, tags, body) => p :: p->env
      case p:ASTNode => p->env
    }
  }
  
  val dim: Choice => Symbol = attr {
    case c: Choice => c.parent[ChoiceExpr].dim
  }    
  
  val bindingInstance: ChoiceExpr => DimensionExpr = attr {
    case p => (p->env).find { _.name == p.dim } match {
      case Some(d:DimensionExpr) => d
      case None => sys error "Free choice : %s!".format(p)
    }
  }
  
  val boundInstances: DimensionExpr => List[ChoiceExpr] = attr {
    case p => p.children.collect {
      case c:ChoiceExpr if c->bindingInstance == p => c
    }.toList
  }
  */
}