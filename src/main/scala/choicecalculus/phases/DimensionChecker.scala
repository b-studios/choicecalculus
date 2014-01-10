package choicecalculus
package phases

import dimensionchecker.DimensionGraph

import lang.ASTNode
import lang.choicecalculus.{ Choice, Alternative, Dimension, Include, 
                             PartialConfig, Select, Share, Identifier }

import org.kiama.rewriting.Rewriter.Term
import org.kiama.attribution.Attribution.attr

import utility.messages._

/**
 * <h2> The DimensionChecker phase
 */
trait DimensionChecker { self: Reader with Namer =>

  /**
   * Decorates the given tree with it's dimensioning
   * 
   * Before running the dimension checker please assure that
   * the [[Namer]] has been run.
   */
  def runDimensionChecker(ast: ASTNode): ASTNode =
    messageScope(phase = 'dimensionchecker) {
      ast->dimensioning
      ast
    }

  /**
   * Computes the dimension graph for a given ASTNode
   */
  val dimensioning: ASTNode => DimensionGraph = 
    messageScope(phase = 'dimensionchecker) {
      attr { (e) => e match {

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
        
        case id@Identifier(name) => id->bindingInstance match {
          case Some(Share(_, boundExpr, _)) => boundExpr->dimensioning
          case _ => raise("Reference to unbound identifier $name.", position = id)
        }
        
        case PartialConfig(body, configs) => (configs.foldLeft(body->dimensioning) {
          case (old, (dim, tag)) => old.select(dim, tag)(e)
        })
        
        case Share(name, boundExpr, body) => body->dimensioning
        
        case inc: Include[_,_] => inc->tree->dimensioning
        
        case Term(p, children) => children.flatMap {
          case n: ASTNode => List(n->dimensioning)
          case l: Seq[ASTNode] => l.map(dimensioning)
          case _ => List.empty
        }.foldLeft(DimensionGraph.empty) {
          case (old, dim) => old.merge(dim) (e)
        }
        
        case _ => DimensionGraph.empty
      }}
    }

}