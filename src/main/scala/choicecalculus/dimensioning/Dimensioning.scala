package choicecalculus
package dimensioning

import org.kiama.util.Messaging.{message, report}
import lang.ASTNode
import lang.choicecalculus.{ Choice, Alternative, Dimension, Include, PartialConfig, Select, Share, Identifier }
import semantics.Includes

import namer.Namer

import utility.AttributableRewriter.Term
import utility.Attribution.attr


trait Dimensioning { self: Includes with Namer =>

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
    case id@Identifier(name) => id->bindingInstance match {
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

}