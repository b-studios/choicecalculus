package choicecalculus
package dimensioning

import org.kiama.util.Messaging.{message, report}
import lang.ASTNode
import lang.choicecalculus.{ Choice, Alternative, Dimension, Include, PartialConfig, Select, Share, Identifier }
import semantics.Includes
import utility.AttributableRewriter.Term
import utility.Attribution.{attr, paramAttr }


trait Dimensioning { self: Includes =>

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
    case Dimension(name, tags, body) => (body->dimensioning).declareDimension(name, tags)(e)
    
    // only toplevel dimensions, which are not dependent can be selected
    case Select(dim, tag, body) => (body->dimensioning).select(dim, tag)(e)    
    
    case Choice(dim, alts) => alts.foldLeft(DimensionGraph.empty) {
      case (old, Alternative(tag, body)) => old.merge((body->dimensioning).fromChoice(dim, tag)(e))(e) 
    }
    
    // gracefully fall back to empty dimension graph
    case Identifier(name) => e->bindingShare(name) match {
      case Some(Share(_, boundExpr, _)) => boundExpr->dimensioning
      case _ => {
        message(e, "ERROR: Use of unbound choice calculus variable '%s'".format(name.name))
        DimensionGraph.empty
      }
    }
    
    case PartialConfig(body, configs) => (configs.foldLeft(body->dimensioning) {
      case (old, (dim, tag)) => old.select(dim, tag)(e)
    })
    
    case Share(name, boundExpr, body) => body->dimensioning
    
    case inc:Include[_,_] => fileDimensions(inc) 
    
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
  val bindingShare: Symbol => ASTNode => Option[Share[_,_]] = paramAttr {
    name => {
      case s@Share(n, _, _) if n == name => Some(s)
      case other if other.isRoot => None
      
      // If the parent is a share expression we have to check whether we are in the binding branch
      // then skip ahead to next parent
      case other => other.parent match {
        case s@Share(_, e, _) if e == other && !s.isRoot => s.parent[ASTNode]->bindingShare(name)
        case otherParent:ASTNode => otherParent->bindingShare(name)
        case _ => None
      }
    }
  }
  
  val bindingDimension: Symbol => ASTNode => Dimension[_] = paramAttr {
    name => {
      case d@Dimension(n, _ ,_) if n == name => d
      case other if other.isRoot => sys error "Cannot find a binding for %s".format(name)
      case other => other.parent[ASTNode]->bindingDimension(name)
    }
  }
}