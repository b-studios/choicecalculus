package choicecalculus
package semantics

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
   */
  case class Dimension(name: Symbol, dims: Map[Symbol, DimensionType]) extends DimensionType {
    override def toString =
      "%s<%s>".format(name.name, dims.map( (d) => "%s: %s".format(d._1.name, d._2)) mkString ", ")
  }
  
  /**
   * Represents A.a → B
   */
  case class DependentDimension(dependsOn: DimensionExpr, choice: Symbol, dim: DimensionType) extends DimensionType {
    override def toString =
      "%s.%s → %s".format(dependsOn.name.name, choice.name, dim)
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
  
  /**
   * Because Tillmann told me, that dimensions are like functions we now use an environment to reflect the lexical
   * binding of Dimensions to Choices 
   */
  def typeOf(e: ASTNode, env: Map[Symbol, DimensionExpr]): Set[DimensionType] = e match {
    
    case d@DimensionExpr(name, tags, body) => {
     
      // top down we provide a lexical binding for this dimension to enable checking of `ChoiceExpr`s
      val dimTypes = typeOf(body, env + (name -> d))
     
      // bottom up we aggregate all `DependentDimension`s into one dimension
      // i.e. { A.a → B, A.b → C, ... } and current dimension is A<a,b,c> will be aggregated into 
      //      { A<a: B, b: C, c: ø>, ... }
      // 1. Initialize all tags with a plain dimension
      // 2. override those tags with found dimensions
      val dependentDims = tags.map { (t) => (t, Plain) } ++ (for {
        DependentDimension(dep, choice, dim) <- dimTypes
        if dep == d
      } yield (choice, dim))
      
      // 3. We now merge the rest of the dimensions with our singleton set
      merge(dimTypes.filter { 
        case DependentDimension(dep, _, _) => dep != d
        case _ => true
      }, Set(Dimension(name, Map(dependentDims.toList: _*))))
    }
    
    /**
     * Double check if this is right, because of dynamic scope!
     */
    case SelectExpr(dim, tag, body) => {
      val dims = typeOf(body, env)
      
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
      
      // 1. Check whether the choice is lexically bound by a dimension
      if (!(env contains dim))
        sys error "unbound choice %s".format(dim.name)
      
      val bindingInstance = env(dim)
        
      // 2. Check whether the choices are exhaustive - currently not using wildcards
      if (bindingInstance.tags.toSet != choices.map(_.tag).toSet)
        sys error "Choices for %s are not exhaustive!".format(dim.name)
      
      // 3. Create DimensionTypes for every branch and make them dependent.
      (for {
        Choice(tag, body) <- choices
        dimType <- typeOf(body, env)
      } yield DependentDimension(env(dim), tag, dimType)).toSet
    }
    case BinaryExpr(lhs, rhs) => merge(typeOf(lhs, env), typeOf(rhs, env))
    case UnaryExpr(body) => typeOf(body, env)
    case c:ConstantExpr => Set(Plain)
    
  }
  
}