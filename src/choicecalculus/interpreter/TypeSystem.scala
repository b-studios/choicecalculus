package choicecalculus
package semantics

/**
 * TODO next things to do: reimplement the exhaustive and binding (dim) checks!
 */
trait TypeSystem {

  import ast._
  
  abstract class DimensionType
  
  /**
   * Represents ø
   */
  case object Plain extends DimensionType {
    override def toString = "ø"
  }
  
  /**
   * Represents A<a: B<...>, b: ø>
   * 
   * I guess the inner dimensions can also be a set like A<a: {B, C, D}, b: ø> since multiple dimensions can
   * depend on one particular choice
   */
  case class Dimension(name: Symbol, dims: Map[Symbol, DimensionType]) extends DimensionType {
    override def toString =
      "%s<%s>".format(name.name, dims.map( (d) => "%s: %s".format(d._1.name, d._2)) mkString ", ")
  }
  
  /**
   * Represents A.a → B
   */
  case class DependentDimension(dim: Symbol, tag: Symbol, dependent: DimensionType) extends DimensionType {
    override def toString =
      "%s.%s → %s".format(dim.name, tag.name, dependent)
  }
  
  /**
   * Depending on the choice of *merging* we have to merge here or throw an error, if a dimension occurs 
   * multiple times
   */
  def merge(lhs: Set[DimensionType], rhs: Set[DimensionType]): Set[DimensionType] = {
    for {
      d1@Dimension(lname, _) <- lhs
      d2@Dimension(rname, _) <- rhs
      if lname == rname
    } sys error "multiple dimension declarations at one level: \n  %s\n  %s".format(d1, d2)
    
    lhs ++ rhs
  }
  
  def dimensionCheck(e: ASTNode): Set[DimensionType] = {
    val dims = dimensionOf(e, Map.empty)
    
    // This should never happen, since it should be caught by the dimension-checker beforehand
    if (dims.filter { _.isInstanceOf[DependentDimension] }.size > 0)
      sys error "Free choices, please add required dims around this expression"
    
    if (dims != Set(Plain))
      dims - Plain
    
    else
      dims
  }
  
  /**
   * Because Tillmann told me, that dimensions are like functions we now use an environment to reflect the lexical
   * binding of Dimensions to Choices
   * 
   * The program is fully configured if this function returns Set(ø). Otherwise every element of the return set
   * gives one possible configuration (except ø)
   */
  def dimensionOf(e: ASTNode, env: Map[Symbol, Set[DimensionType]]): Set[DimensionType] = e match {
    
    case d@DimensionExpr(name, tags, body) => {
     
      val dimTypes = dimensionOf(body, env)
      
      // bottom up we aggregate all `DependentDimension`s into one dimension
      // i.e. { A.a → B, A.b → C, ... } and current dimension is A<a,b,c> will be aggregated into 
      //      { A<a: B, b: C, c: ø>, ... }
      // 1. Initialize all tags with a plain dimension
      // 2. override those tags with found dimensions
      val dependentDims = for {
        DependentDimension(dim, tag, dependent) <- dimTypes
        if dim == name
      } yield (tag, dependent)
      
      // Here may be a good point to check whether any choice is using this dimension. If this is not the case
      // a warning should be printed.
      if (dependentDims.size == 0)
        printf("Your declared dimension %s is never used. Maybe remove it?\n", name.name)
      
      
      // 3. We now merge the rest of the dimensions with our singleton set
      merge(dimTypes.filter { 
        case DependentDimension(dim, _, _) => dim != name
        case _ => true
      }, Set(Dimension(name, Map((tags.map { (t) => (t, Plain) } ++ dependentDims).toList: _*))))
    }
    
    /**
     * Double check if this is right, because of dynamic scope!
     */
    case SelectExpr(dim, tag, body) => {
      
      val dims = dimensionOf(body, env)
      
      val selected = dims.map {
        // select A.b from (dim A<a,b> in A<a: 1, b: 2>) ~> 2
        case Dimension(name, dims) if name == dim => dims(tag)
        case d:Dimension => d
      
        // select A.e from B<a: dim A<e,f> in ...> - we certainly don't want to select this A
        case d:DependentDimension => d
        
        case Plain => Plain 
      }
      
      // here we also should search for dependent dimensions and add this information to the warning
      if (selected == dims)
        printf("Warning: your selection %s.%s is vacuous - maybe there is something bad going on\n", dim.name, tag.name)
      
      selected
    }
    
    // For every choice recursively invoke `typeOf` and then make those inner dimensions dependent of the choice
    case ChoiceExpr(dim, choices) => {
      
      /**
       * This cannot be done anymore, because now we use the env for the share construct
       *
      // 1. Check whether the choice is lexically bound by a dimension
      if (!(env contains dim))
        sys error "unbound choice %s".format(dim.name)
      
      val bindingInstance = env(dim)
        
      // 2. Check whether the choices are exhaustive - currently not using wildcards
      if (bindingInstance.tags.toSet != choices.map(_.tag).toSet)
        sys error "Choices for %s are not exhaustive!".format(dim.name)
      */
      // 3. Create DimensionTypes for every branch and make them dependent.
      (for {
        Choice(tag, body) <- choices
        dimType <- dimensionOf(body, env)
      } yield DependentDimension(dim, tag, dimType)).toSet
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