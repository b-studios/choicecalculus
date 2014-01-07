package choicecalculus
package semantics

import lang.ASTNode
import org.kiama.util.{ Compiler }
import org.kiama.rewriting.Strategy
import utility.DebugRewriter._
import utility.{ Messaging, ParserUtils }
import utility.Attribution.initTree
import dimensioning.{ DimensionGraph, Dimensioning }

import namer.Namer

trait Semantics extends Dimensioning
    with Namer
    with Selecting     
    with Choosing 
    with Substituting
    with Includes { self: Compiler[ASTNode] with ParserUtils =>
  
  // Our reduction strategy
  // top down traversal breaks attribution links to parents, so lookup of bindings cannot be performed
  def reduce(s: Strategy): Strategy = repeat(oncebu(s))
    
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