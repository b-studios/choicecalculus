package choicecalculus
package semantics

import ast._
import org.kiama.rewriting.Rewriter._
import org.kiama.util.Messaging.message

trait Semantics extends Dimensioning with DimensionGraph 
    with Selecting     
    with Choosing 
    with Substituting {

}
  
 

trait Dimensioning { self: DimensionGraph =>
  
  import org.kiama.attribution.Attribution.{attr, paramAttr, CachedParamAttribute}

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
    
    case Term(p, children) => children.collect {
      case n: ASTNode => n->dimensioning
    }.foldLeft(DimensionGraph.empty) {
      case (old, dim) => old.merge(dim) (e)
    }
    
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

trait Selecting { self: Choosing => 
  
  import ast._
  import org.kiama.util.Messaging.message
  
  import org.kiama.rewriting.Rewriter._  
 
  /**
   * We do the traversal bottomup to perform inner selection first 
   */
  val select = bottomup (reduce (selectRelation))
  
  val selectRelation = rule {
    
    // Don't select dependent dimensions!
    case SelectExpr(_, _, c:ChoiceExpr) => c
    
    case SelectExpr(dim, tag, DimensionExpr(name, tags, body)) if name == dim =>
      rewrite (choose(dim, tag)) (body)
  
    case SelectExpr(dim, tag, id:IdExpr) => PartialConfig(id, List((dim, tag)))
  
    case SelectExpr(dim, tag, PartialConfig(body, configs)) => PartialConfig(body, configs ++ List((dim, tag)))
  
    // Congruences  
    case SelectExpr(dim, tag, ShareExpr(name, expr, body)) => 
      ShareExpr(name, expr, SelectExpr(dim, tag, body))
  
    case SelectExpr(dim, tag, DimensionExpr(name, tags, body)) if name != dim => 
      DimensionExpr(name, tags, SelectExpr(dim, tag, body)) 
  
    // Hostlanguage constructs - wrap every child into selectexpressions and reconstruct node
    case SelectExpr(dim, tag, t) => rewrite ( all ( rule {
      case n:ASTNode => SelectExpr(dim, tag, n)
      case lit => lit
    })) (t)
  }
}


/**
 * 2. Step - Substitution and Removal of unnecessary Shares
 */
trait Substituting { self: Selecting with Dimensioning =>
  
  import ast._
  import org.kiama.rewriting.Rewriter._
  
  // If bindings are substituted, then we have to reduce them again
  // afterwards a cleanup step is performed
  val substitute = bottomup ( attempt( substituteBindings <* attempt(select) ) <* attempt(removeShares) )
  
  
  // We only substitute variables, if they are fully configured
  private val substituteBindings = 
    test(isFullyConfigured) <* (substIdExpr + substPartialConfig) <* desugarPartialConfig

  private val isFullyConfigured = strategyf {
    case exp: ASTNode if (exp->dimensioning).fullyConfigured => Some(exp)
    case _ => None
  }
  
  // (a) the bound expression itself is fully configured, then the id can be substituted by the expression
  private val substIdExpr = rule {
    case id@IdExpr(name) => id->bindingShare(name) match {
      case Some(ShareExpr(_, binding, _)) => binding
      case _ => sys error "cannot substitute binding for %s".format(name)
    }
  }
  
  // (b) the bound expression is fully configured by delayed selections
  private val substPartialConfig = rule {
    case PartialConfig(id: IdExpr, configs) => PartialConfig( rewrite(substIdExpr) (id), configs)
  }
  
  // The expression has been substituted by one of the previous steps - just desugar the partial configuration
  private val desugarPartialConfig = rule {
    case PartialConfig(body, configs) => configs.foldLeft(body) {
      case (old, (dim, tag)) => SelectExpr(dim, tag, old)
    }
  }  

  // Unused shares can be removed.
  private val removeShares = rule {
    case ShareExpr(n,_,body) if !(body->variableIsUsed(n)) => body
  }
}


/**
 * In this relation we use function definition instead of syntactic representation to omit introduction of yet
 * another syntactic element. Can easily be rewritten.
 * 
 * Since the congruence rules match completely kiama's default behavior we can use `sometd(rule {...})` instead
 * of implementing all congruences by ourselves. `sometd` will try to continue on subtrees until it successfully 
 * matches a rule. Then it will stop processing this subtree.
 */
trait Choosing {
 
  def choose(dim: Symbol, tag: Symbol) = sometd ( rule {
    
    // select expressions shadow other ones, with the same dimension
    case e@SelectExpr(d, _, _) if d == dim => e
    
    // selection stops at dimensions with the same name
    case e@DimensionExpr(d, _, _) if d == dim => e
    
    // actual choosing 
    case ChoiceExpr(d, choices) if d == dim => choices.collect {
      case Choice(t, body) if t == tag => body
    }.head
    
  })  
 
}