package choicecalculus
package semantics

import ast.{ ASTNode, DimensionExpr, SelectExpr, ChoiceExpr, IdExpr, Choice, ShareExpr, PartialConfig, IncludeExpr }
import utility.AttributableRewriter.Term
import org.kiama.util.Messaging.{message, report}
import org.kiama.attribution.UncachedAttribution.{attr, paramAttr }

trait Dimensioning { self: DimensionGraph with Includes =>

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
    
    case ShareExpr(name, boundExpr, body) => body->dimensioning
    
    case inc:IncludeExpr[_,_] => fileDimensions(inc) 
    
    case Term(p, children) => children.flatMap {
      case n:ASTNode => List(n->dimensioning)
      case l:Seq[ASTNode] => l.map(dimensioning)
      case _ => List.empty
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
  val bindingShare: Symbol => ASTNode => Option[ShareExpr[_,_]] = paramAttr {
    name => {
      case s@ShareExpr(n, _, _) if n == name => Some(s)
      case other if other.isRoot => None
      
      // If the parent is a share expression we have to check whether we are in the binding branch
      // then skip ahead to next parent
      case other => other.parent match {
        case s@ShareExpr(_, e, _) if e == other && !s.isRoot => s.parent[ASTNode]->bindingShare(name)
        case otherParent:ASTNode => otherParent->bindingShare(name)
        case _ => None
      }
    }
  }
  
  val bindingDimension: Symbol => ASTNode => DimensionExpr[_] = paramAttr {
    name => {
      case d@DimensionExpr(n, _ ,_) if n == name => d
      case other if other.isRoot => sys error "Cannot find a binding for %s".format(name)
      case other => other.parent[ASTNode]->bindingDimension(name)
    }
  }
}