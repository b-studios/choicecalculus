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
trait DimensionChecker { self: ChoiceGraph =>
  
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
      reduce( selectFromChoices(dim, tag) )(body) match {
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
    case SelectExpr(dim, tag, body) => sometd( selectFromDimension(dim, tag) )(body) match {
      case Some(result) => result
      case _ => None // maybe throw error here
    }
  }
  
  def eval = reduce ( selectReduction )
  
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
  
  /**
   * Lexical environment of bound dimensions
   * Code adapted from kiama's "lambda2" example
   */
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
  
  def getChoiceGraph(tree: ASTNode): ChoiceGraph = tree->choiceGraph
  
  // If the choice graph can be calculated there are no dependend choices that create cycles
  // choiceGraphs are created bottom up
  val choiceGraph: ASTNode => ChoiceGraph = attr {
    
    case ChoiceExpr(dim, choices) => choices.map {
      case Choice(tag, body) => (body->choiceGraph).from(dim, tag)
    }.foldLeft(ChoiceGraph.empty) { _ ++ _ }
    case DimensionExpr(_,_, body) => body->choiceGraph

    case SelectExpr(dim, tag, body) => (body->choiceGraph).select(dim, tag)
    
    case BinaryExpr(lhs, rhs) => (lhs->choiceGraph) ++ (rhs->choiceGraph)
    case UnaryExpr(body) => body->choiceGraph
    case _:ConstantExpr => ChoiceGraph.empty
    
  }
  
  def merge(lhs: Set[Dimension], rhs: Set[Dimension]): Set[Dimension] = {
    
    // only check if the names of the dimensions collide
    val intersect = for {
      dim1 <- lhs
      dim2 <- rhs
      if (dim1.name == dim2.name)
    } yield (dim1, dim2)
    
    val joined = for {
      (lhs, rhs) <- intersect
    } yield {
     if (lhs == rhs)
       lhs
     // here a warning could be emitted telling that joining takes place
     // those warnings could be controlled by a configuration flag
     else 
       join(lhs, rhs)
    }
    
    joined ++ (lhs -- intersect.map( _._1 )) ++ (rhs -- intersect.map( _._2 ))    
  }
  
  // Remember: Can only join Dimensions with identical names
  def join(lhs: Dimension, rhs: Dimension): Dimension = (lhs, rhs) match {
    
    case (WildcardDim(name1, tags1), WildcardDim(name2, tags2)) => 
      Dimension(name1, tags1 ++ tags2)
      
    case (WildcardDim(name1, tags1), Dimension(name2, tags2)) => 
      Dimension(name1, tags2)
      
    case (Dimension(name1, tags1), WildcardDim(name2, tags2)) => 
      Dimension(name1, tags1)
      
    case (Dimension(name1, tags1), Dimension(name2, tags2)) => 
      Dimension(name1, tags1 & tags2)
  }
  
  // TODO Implement
  def canSelect(dim: Symbol, tag: Symbol, dimensions: Set[Dimension]) = true
  
  def dimensionOf(e: ASTNode): Set[Dimension] = e match {
    
  
    /*
     *            Г, D<a,b,c> ⊢ e : Δ
     * ——————————————————————————————————————
     * Г ⊢ dim D<a,b,c> in e : D<a,b,c> ⊕ Δ
     */
    case DimensionExpr(name, tags, body) => merge(dimensionOf(body), Set(Dimension(name, tags.toSet)))
    
    /*
     * According to Problem (5) a usage of `choice`
     * with fewer choices than declared will raise
     * an error.
     * 
     *             D<a,b,c> ∈ Г 
     * ——————————————————————————————————————
     * Г ⊢ D<a:x1, b:x2, c:x3> : { D<a,b,c> }
     * 
     * TODO Implement this error raising at dimensioning time
     */
    case ChoiceExpr(dim, choices) => choices.foldLeft(Set[Dimension]()){ 
      (old, choice) => merge(old, dimensionOf(choice))
    }
    case Choice(tag, body) => dimensionOf(body)
    
    /* 
     *   Г ⊢ e : D<a,b,c> ⊕ Δ
     * ———————————————————————
     * select D.t from e : Δ_i 
     */
    case SelectExpr(dim, tag, body) =>  {
      val dims = dimensionOf(body)
      if (!canSelect(dim, tag, dims))
        sys error "Cannot select %s.%s from %s".format(dim, tag, dims)
      
        // ERROR This is not the correct implementation - what about dependent choices?????
      dims -- dims.filter( _.name == dim )
    }
    
    // Host level constrcuts
    case BinaryExpr(lhs, rhs) => merge(dimensionOf(lhs), dimensionOf(rhs))
    case UnaryExpr(content) => dimensionOf(content)    
    
    /*
     *  ———————————————————————
     *  ⊢ HostlanguageExpr : {}
     */
    case _ => Set()
  }
  
  
  
  /*
   * 
   * Г ⊢ e: Δ1    Г, v:Δ1 ⊢ b : Δ2
   * ——————————————————————————————
   *  Г ⊢ share v = e in b : Δ2
   */
}