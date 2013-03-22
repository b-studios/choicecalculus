package choicecalculus
package semantics

trait TypeSystemRevised {

  import ast._
  
  abstract class DimensionNode
  
  type DimensionGraph = Set[DimensionNode]
  
  // searches all concrete dimensions of the graph
  // since there is at most one level of unbound choices and we are only interested in the first
  // layer this works fine
  def allDims(graph: DimensionGraph): DimensionGraph =
    (for (dim <- graph) yield dim match {
      case UnboundDependency(_,_,dims) => dims
      case d => Set(d)
    }).flatten
      
  
  /**
   * Represents ø
   */
  case object Plain extends DimensionNode {
    override def toString = "ø"
  }
  
  /**
   * Represents A<a: B<...>, b: ø>
   * Every dimension can contain a DimensionGraph as dependent 
   */
  case class Dimension(name: Symbol, dependentDims: Map[Symbol, Set[DimensionNode]]) extends DimensionNode {
    override def toString =
      "%s<%s>".format(name.name, dependentDims.map( (d) => "%s: %s".format(d._1.name, d._2)) mkString ", ")
  }
  
  case class UnboundDependency(dim: Symbol, tag: Symbol, dependentDims: Set[DimensionNode]) extends DimensionNode {
    override def toString =
      "%s.%s → %s".format(dim.name, tag.name, dependentDims mkString ", ")
  }
  
  /**
   * Depending on the choice of *merging* we have to merge here or throw an error, if a dimension occurs 
   * multiple times
   */
  def merge(lhs: DimensionGraph, rhs: DimensionGraph): DimensionGraph = {
    for {
      d1@Dimension(lname, _) <- lhs
      d2@Dimension(rname, _) <- rhs
      if lname == rname
    } sys error "multiple dimension declarations at one level: \n  %s\n  %s".format(d1, d2)
    
    lhs ++ rhs
  }
  
  /**
   * `env` is used to keep track of shared expressions 
   */
  def dimensionOf(e: ASTNode, env: Map[Symbol, DimensionGraph]): DimensionGraph = e match {

    
    case DimensionExpr(name, tags, body) => {
      
      val dims = dimensionOf(body, env)
      
      // find all dependecies, that are in the lexical scope of this dimension declaration
      // i.e. { A.a → B, A.b → C, ... } and current dimension is A<a,b,c> will be aggregated into 
      //      { A<a: B, b: C, c: ø>, ... }
      val dependentDims = for {
        UnboundDependency(dim, tag, dependent) <- dims
        if dim == name
        
      // here it could also be checked whether `tag` is a declared one
      } yield (tag, dependent)
      
      if (dependentDims.size == 0)
        printf("Your declared dimension %s is never used. Maybe remove it?\n", name.name)
      
      // To include all possible selections (not just the ones with dependent dimensions, also add plain dimensions for
      // all other tags
      val plainAlternatives: Map[Symbol, DimensionGraph] = Map(tags.map { (t) => (t, Set[DimensionNode](Plain)) }: _*)
      
      // We now merge the rest of the dimensions with our singleton set.  
      merge(dims.filter { 
        // all but the ones, bound by the dimension declaration
        case UnboundDependency(dim, _, _) => dim != name
        case _ => true
      }, Set(Dimension(name, plainAlternatives ++ Map((dependentDims.toList): _*))))
    }

    
    // only toplevel dimensions, which are not dependent can be selected
    case SelectExpr(dim, tag, body) => {
      val dims = dimensionOf(body, env)
      
      val selected = dims.flatMap[DimensionNode, DimensionGraph] {
        case Dimension(name, dependendDims) if name == dim => dependendDims(tag)
        case other => Set(other)
      }
      
      // here we also should search for dependent dimensions and add this information to the warning
      if (selected == dims)
        printf("Warning: your selection %s.%s is vacuous - maybe there is something bad going on\n", dim.name, tag.name)
      
      selected
    }
    
    // For every choice recursively invoke `typeOf` and then make those inner dimensions dependent of the choice
    // I HAVE TO USE REFERENCES (not value types) TO MAKE THIS WORK EFFICIENTLY
    case ChoiceExpr(dim, choices) => {
      
      (for (Choice(tag, body) <- choices) yield {
        val dims = dimensionOf(body, env)
        
        // all previously unbound dependency stay that way. we just don't want the other dimensions anymore, since
        // now they are dependent on the newly created choice
        dims.filter { _.isInstanceOf[UnboundDependency] } + UnboundDependency(dim, tag, allDims(dims))
      }).flatten.toSet
    }
    
    // here caching should happen (especially with files / modules)
    case ShareExpr(name, boundExpr, body) => dimensionOf(body, env + (name -> dimensionOf(boundExpr, env)))
    
    case IdExpr(name) => if (env contains name)
        env(name)
      else
        sys error "Use of unbound choice calculus variable %s".format(name)
        
    case BinaryExpr(lhs, rhs) => merge(dimensionOf(lhs, env), dimensionOf(rhs, env))
    case UnaryExpr(body) => dimensionOf(body, env)
    case c:ConstantExpr => Set(Plain)    
  }
}