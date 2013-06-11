package choicecalculus
package semantics

import ast._
import org.kiama.util.{ PositionedParserUtilities, Compiler }
import org.kiama.attribution.Attribution.initTree
import utility.AttributableRewriter._

trait Semantics extends Dimensioning 
    with DimensionGraph 
    with Selecting     
    with Choosing 
    with Substituting
    with Includes { self: Compiler[ASTNode] with PositionedParserUtilities =>
  
  def processTree(tree: ASTNode): ASTNode = {
    initTree(tree)
    tree->dimensioning
    rewrite (select then substitute) (tree)
  }      
}