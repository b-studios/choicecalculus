package choicecalculus
package semantics

import ast.{ ASTNode, IdExpr, IncludeExpr, PartialConfig, SelectExpr, ShareExpr }
import utility.AttributableRewriter.{ attempt, allbu, alltd, bottomup, reduce, repeat, rewrite, rule, strategyf, test }
import org.kiama.attribution.Attribution.{ paramAttr } 
    

/**
 * 2. Step - Substitution and Removal of unnecessary Shares
 */
trait Substituting { self: Selecting with Dimensioning with Includes =>
  
  // If bindings are substituted, then we have to reduce them again
  // afterwards a cleanup step is performed
  val substitute = bottomup(reduce(substituteBindings <* select) andFinally removeShares)
  
  
  // We only substitute variables, if they are fully configured
  val substituteBindings = 
    test(isFullyConfigured) <* ((substIdExpr + substIncludeExpr + substPartialConfig) andFinally desugarPartialConfig)

  val isFullyConfigured = strategyf {
    case exp: ASTNode if (exp->dimensioning).fullyConfigured => Some(exp)
    case exp: ASTNode => None
    case _ => None
  }
  
  // (a) the bound expression itself is fully configured, then the id can be substituted by the expression
  // i. It's an id
  val substIdExpr = rule {
    case id@IdExpr(name) => id->bindingShare(name) match {
      case Some(ShareExpr(_, binding, _)) => binding
      case other => sys error "cannot substitute binding for %s, got %s".format(name, other)
    }
  }
  // ii. It's an include
  val substIncludeExpr = rule {
    case inc:IncludeExpr[_,_] => fileContents(inc)
  }
  
  // (b) the bound expression is fully configured by delayed selections
  val substPartialConfig = rule {
    case PartialConfig(body, configs) =>
      PartialConfig( rewrite(substIdExpr + substIncludeExpr) (body), configs)
  }
  
  // The expression has been substituted by one of the previous steps - just desugar the partial configuration
  val desugarPartialConfig = rule {
    case PartialConfig(body, configs) => configs.foldLeft(body) {
      case (old, (dim, tag)) => SelectExpr(dim, tag, old)
    }
  }  

  // Unused shares can be removed.
  private val removeShares = rule {
    case ShareExpr(n,_,body) if !(body->variableIsUsed(n)) => body
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
}

