package choicecalculus
package semantics

import ast._
import org.kiama.util.{ PositionedParserUtilities, Compiler }
import org.kiama.attribution.UncachedAttribution.initTree
import org.kiama.rewriting.Strategy
import utility.DebugRewriter._
import utility.Messaging
import dimensioning.{ DimensionGraph, Dimensioning }
trait Semantics extends Dimensioning
    with Selecting     
    with Choosing 
    with Substituting
    with Includes { self: Compiler[ASTNode] with PositionedParserUtilities =>
  
  // Our reduction strategy
  // top down traversal breaks attribution links to parents, so lookup of bindings cannot be performed
  def reduce(s: Strategy): Strategy = repeat(manybu(s))
      
  def processTree(tree: ASTNode): Either[String, ASTNode] = {
    initTree(tree)
    
    Messaging.resetmessages
    tree->dimensioning
    if (Messaging.errorcount > 0) {
      Left("Errors occured while dimensioning")
    } else {      
      Right(rewrite(reduce(select + substitute + removeShares)) (tree))
    }
  }
  
  def performSelection(tree: ASTNode): ASTNode = {
    initTree(tree)
    tree->dimensioning
    rewrite(reduce(select)) (tree)
  }
  
  def performSubstitution(tree: ASTNode): ASTNode = {
    initTree(tree)
    tree->dimensioning
    rewrite(reduce(substitute + removeShares)) (tree)
  }
}