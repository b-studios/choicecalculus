package choicecalculus
package semantics

import ast.{ ASTNode, IdExpr, IncludeExpr, PartialConfig, SelectExpr, ShareExpr }
import dimensioning.Dimensioning
import utility.DebugRewriter._
import utility.Attribution.{ paramAttr } 
import org.kiama.rewriting.Strategy

/**
 * 2. Step - Substitution
 * ----------------------
 * Substitution of bindings (`substitute`) and removal of unnecessary shares (`removeShares`).
 */
trait Substituting { self: Selecting with Dimensioning with Includes =>

  // We only substitute variables, if they are fully configured
  lazy val substitute = named("substitute",
    test(isFullyConfigured) <* (substIdExpr + substIncludeExpr + (substPartialConfig <* desugarPartialConfig))
  )
  lazy val isFullyConfigured = strategyf("isFullyConfigured?", {
    case exp: ASTNode if (exp->dimensioning).fullyConfigured => Some(exp)
    case exp: ASTNode => None
    case _ => None
  })
  
  // (a) the bound expression itself is fully configured, then the id can be substituted by the expression
  // i. It's an id
  lazy val substIdExpr = rule("substIdExpr", {
    case id@IdExpr(name) => id->bindingShare(name) match {
      case Some(ShareExpr(_, binding, _)) => /*resetMemo;*/ binding.clone
      case other => sys error "cannot substitute binding for %s, got %s".format(name, other)
    }
  })
  
  // ii. It's an include
  lazy val substIncludeExpr = rule ("substIncludeExpr", {
    case inc:IncludeExpr[_,_] =>  /*resetMemo;*/ fileContents(inc).clone
  })
  
  // (b) the bound expression is fully configured by delayed selections
  // TODO use kiama's `congruence` rule for this
  lazy val substPartialConfig = rule("substPartialConfig", {
    case PartialConfig(body:IdExpr[_], configs) =>      
      PartialConfig( rewrite(substIdExpr) (body), configs)
      
    case PartialConfig(body:IncludeExpr[_,_], configs) =>      
      PartialConfig( rewrite(substIncludeExpr) (body), configs)
  })
  
  // The expression has been substituted by one of the previous steps - just desugar the partial configuration
  lazy val desugarPartialConfig = rule("desugarPartialConfig", {
    case PartialConfig(body, configs) => configs.foldLeft(body) {
      case (old, (dim, tag)) => SelectExpr(dim, tag, old)
    }
  })

  // Unused shares can be removed.
  lazy val removeShares = rule("removeShares", {
    case ShareExpr(n,_,body) if !(body->variableIsUsed(n)) => body
  })
  
  lazy val variableIsUsed: Symbol => ASTNode => Boolean = paramAttr {
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