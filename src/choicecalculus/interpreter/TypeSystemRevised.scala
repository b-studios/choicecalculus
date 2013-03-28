package choicecalculus
package semantics

trait TypeSystemRevised { self: DimensionGraph =>

  /**
   * TODO understand topdownS with stop criterion and use it to control traversal
   * 
   */
  
  
  import ast._
  import org.kiama.util.Messaging.message
  import org.kiama.attribution.Attribution.{attr, paramAttr, CachedParamAttribute}
  import org.kiama.rewriting.Rewriter._  
 
  
  /**
   * 1. Step: Perform all selections
   * -------------------------------
   */
  
  // Don't select dependent dimensions!
  val selectFromChoice = rule {
    case SelectExpr(_, _, c:ChoiceExpr) => c
  }
  val selectFromDim = rule {
    case SelectExpr(dim, tag, DimensionExpr(name, tags, body)) if name == dim =>
      rewrite (chooseRelation(dim, tag)) (body)
  }
  val selectFromId = rule {
    case SelectExpr(dim, tag, id:IdExpr) => PartialConfig(id, List((dim, tag)))
  }  
  val selectFromPartialConfig = rule {
    case SelectExpr(dim, tag, PartialConfig(body, configs)) => PartialConfig(body, configs ++ List((dim, tag)))
  }
  
  // Congruences  
  val selectFromShare = rule {
    case SelectExpr(dim, tag, ShareExpr(name, expr, body)) => 
      ShareExpr(name, expr, SelectExpr(dim, tag, body))
  }
  val selectFromOtherDim = rule {
    case SelectExpr(dim, tag, DimensionExpr(name, tags, body)) if name != dim => 
      DimensionExpr(name, tags, SelectExpr(dim, tag, body)) 
  }  
  val selectFromBinary = rule {
    case SelectExpr(dim, tag, b@BinaryExpr(lhs, rhs)) =>
      b.rebuild(SelectExpr(dim, tag, lhs), SelectExpr(dim, tag, rhs))
  }  
  val selectFromUnary = rule {
    case SelectExpr(dim, tag, u@UnaryExpr(content)) =>
      u.rebuild(SelectExpr(dim, tag, content))
  }  
  val selectFromConstant = rule {
    case SelectExpr(dim, tag, c:ConstantExpr) => c
  }

  /**
   * We do the traversal bottomup to perform inner selection first 
   */
  val selectRelation = bottomup (reduce (    
    selectFromChoice + 
    selectFromDim +
    selectFromId +    
    selectFromPartialConfig +
    
    selectFromShare + 
    selectFromOtherDim +
    selectFromBinary +
    selectFromUnary + 
    selectFromConstant
  ))
  
 
  /**
   * In this relation we use function definition instead of syntactic representation to omit introduction of yet
   * another syntactic element. Can easily be rewritten.
   * 
   * Since the congruence rules match completely kiama's default behavior we can use `sometd(rule {...})` instead
   * of implementing all congruences by ourselves. `sometd` will try to continue on subtrees until it successfully 
   * matches a rule. Then it will stop processing this subtree.
   */
  def chooseRelation(dim: Symbol, tag: Symbol) = sometd ( rule {
    
    // select expressions shadow other ones, with the same dimension
    case e@SelectExpr(d, _, _) if d == dim => e
    
    // selection stops at dimensions with the same name
    case e@DimensionExpr(d, _, _) if d == dim => e
    
    // actual choosing 
    case ChoiceExpr(d, choices) if d == dim => choices.collect {
      case Choice(t, body) if t == tag => body
    }.head
    
  })
  
  
  /**
   * 2. Step - Substitution and Removal of unnecessary Shares
   * --------------------------------------------------------
   * If bindings are substituted, then we have to reduce them again
   */  
  val performSubstitution = bottomup ( 
    attempt( substituteBindings <* attempt(selectRelation) ) <* attempt(removeShares)
  )
  
  
  /**
   * We only substitute variables, if they are fully configured
   */  
  val substituteBindings = test(isFullyConfigured) <* (substIdExpr + substPartialConfig) <* desugarPartialConfig

  val isFullyConfigured = strategyf {
    case exp: ASTNode if (exp->dimensioning).fullyConfigured => Some(exp)
    case _ => None
  }
  
  // (a) the bound expression itself is fully configured, then the id can be substituted by the expression
  val substIdExpr = rule {
    case id@IdExpr(name) => id->bindingShare(name) match {
      case Some(ShareExpr(_, binding, _)) => binding
      case _ => sys error "cannot substitute binding for %s".format(name)
    }
  }
  
  // (b) the bound expression is fully configured by delayed selections
  val substPartialConfig = rule {
    case PartialConfig(id: IdExpr, configs) => PartialConfig( rewrite(substIdExpr) (id), configs)
  }
  
  // The expression has been substituted by one of the previous steps - just desugar the partial configuration
  val desugarPartialConfig = rule {
    case PartialConfig(body, configs) => configs.foldLeft(body) {
      case (old, (dim, tag)) => SelectExpr(dim, tag, old)
    }
  }
  
  /**
   * This is a cleanup step. Unused shares can be removed.
   */
  val removeShares = rule {
    case ShareExpr(n,_,body) if !(body->variableIsUsed(n)) => body
  }
  
  
  /**
   * The type parameters are: [ParamType, NodeType, ResultType]
   */
  val dimensioning: ASTNode => DimensionGraph = attr { (e) => e match {

    /**
     * (A) --a--> B<> --b--> C<>
     *    \--b--> D<>
     * 
     * becomes
     * 
     * A<> --a--> B<> --b--> C<>
     *    \--b--> D<>
     */
    case DimensionExpr(name, tags, body) => (body->dimensioning).declareDimension(name, tags)(e)
    
    // only toplevel dimensions, which are not dependent can be selected
    case SelectExpr(dim, tag, body) => (body->dimensioning).select(dim, tag)(e)    
    
    case ChoiceExpr(dim, choices) => choices.foldLeft(DimensionGraph.empty) {
      case (old, c@Choice(tag, body)) => old.merge((body->dimensioning).fromChoice(dim, tag)(e))(e) 
    }
    
    // gracefully fall back to empty dimension graph
    case IdExpr(name) => e->bindingShare(name) match {
      case Some(ShareExpr(_, boundExpr, _)) => boundExpr->dimensioning
      case _ => {
        message(e, "ERROR: Use of unbound choice calculus variable '%s'".format(name.name))
        DimensionGraph.empty
      }
    }
    
    case PartialConfig(body, configs) => (configs.foldLeft(body->dimensioning) {
      case (old, (dim, tag)) => old.select(dim, tag)(e)
    })
    
    // Just some congruence rules
    case ShareExpr(name, boundExpr, body) => body->dimensioning
    case BinaryExpr(lhs, rhs) => (lhs->dimensioning).merge(rhs->dimensioning)(e)
    case UnaryExpr(body) => body->dimensioning
    case _ => DimensionGraph.empty
  }}
  
  /**
   * searches the share expr which provides the binding for the given symbol
   * works lexical & bottom up
   * 
   * ATTENTION: There might be a problem with free variables in shared expressions like
   * 
   * 		share v = 2 in 
   *      share v = v in
   *        v
   * 
   * Here the second binding of v appears to be circular, which is wrong!
   */
  val bindingShare: Symbol => ASTNode => Option[ShareExpr] = paramAttr {
    name => {
      case s@ShareExpr(n, _, _) if n == name => Some(s)
      case other if other.isRoot => None
      
      // If the parent is a share expression we have to check whether we are in the binding branch
      // then skip ahead to next parent
      // TODO check whether parent is root and a grand parent can exist!
      case other => other.parent match {
        case s@ShareExpr(_, e, _) if e == other => s.parent[ASTNode]->bindingShare(name)
        case otherParent:ASTNode => otherParent->bindingShare(name)
      }
    }
  }
  
  val variableIsUsed: Symbol => ASTNode => Boolean = paramAttr {
    name => {
      case IdExpr(n) => n == name
      // shadowed
      case ShareExpr(n,_,_) if n == name => false
      case other => other.children.foldLeft(false) {
        case (old, node:ASTNode) => old || node->variableIsUsed(name)
      }
    }
  }
  
  // this one is easier then bindingShare
  val bindingDimension: Symbol => ASTNode => DimensionExpr = paramAttr {
    name => {
      case d@DimensionExpr(n, _ ,_) if n == name => d
      case other if other.isRoot => sys error "Cannot find a binding for %s".format(name)
      case other => other.parent[ASTNode]->bindingDimension(name)
    }
  }
  
 
}