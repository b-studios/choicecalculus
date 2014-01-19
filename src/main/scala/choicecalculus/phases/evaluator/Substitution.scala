package choicecalculus
package phases
package evaluator

import lang.trees._

import org.kiama.attribution.Attribution.{ paramAttr }
import org.kiama.rewriting.Rewriter

import utility.messages._

/**
 * 2. Step - Substitution
 * ----------------------
 * Substitution of bindings (`substitute`) and removal of unnecessary shares
 * (`removeShares`).
 */
trait Substitution { self: Reader with Namer with DimensionChecker with Rewriter =>

  // We only substitute variables, if they are fully configured
  lazy val substitute =
    test(isFullyConfigured) <* (substIdExpr + substIncludeExpr + (substPartialConfig <* desugarPartialConfig))

  // TODO also check whether exp is an Identifier, a PartialConfig or an Include
  // dimensioning is performed muted in order to avoid redundant messages
  lazy val isFullyConfigured = strategyf("isFullyConfigured", {
    case exp: Tree if (mute { exp->dimensioning }).fullyConfigured => Some(exp)
    case _ => None
  })

  // (a) the bound expression itself is fully configured, then the id can be substituted by the expression
  // i. It's an id
  lazy val substIdExpr = rule("substIdExpr", {
    case id @ Identifier(name) => (id->symbol).definition match {
      case Share(_, binding, _) => binding
    }
  })

  // ii. It's an include
  lazy val substIncludeExpr = rule("substIncludeExpr", {
    case inc: Include => inc->tree
  })

  // (b) the bound expression is fully configured by delayed selections
  lazy val substPartialConfig = rule("substPartialConfig", {
    case PartialConfig(body: Identifier, configs) =>
      PartialConfig(rewrite(substIdExpr)(body), configs)

    case PartialConfig(body: Include, configs) =>
      PartialConfig(rewrite(substIncludeExpr)(body), configs)
  })

  // The expression has been substituted by one of the previous steps - just desugar the partial configuration
  lazy val desugarPartialConfig = rule("desugarPartialConfig", {
    case PartialConfig(body, configs) => configs.foldLeft(body) {
      case (old, (dim, tag)) => Select(dim, tag, old)
    }
  })

  // Unused shares can be removed.
  lazy val removeShares = rule("removeShares", {
    case Share(n, _, body) if !(body->variableIsUsed(n)) => body
  })

  lazy val variableIsUsed: Symbol => Tree => Boolean = paramAttr {
    name => {
      case Identifier(n) => n == name
      // shadowed
      case Share(n, _, _) if n == name => false
      case other => other.children.foldLeft(false) {
        case (old, node: Tree) => old || node->variableIsUsed(name)
      }
    }
  }
}