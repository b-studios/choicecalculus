package choicecalculus
package phases

import evaluator.{ Selection, Substitution }
import lang.ASTNode

import org.kiama.rewriting.Rewriter.{ oncebu, rewrite, repeat }

import org.kiama.rewriting.Strategy

import utility.messages._

/**
 * <h2> The Evaluator phase
 */
trait Evaluator extends Selection with Substitution { 
  self: Reader with Namer with DimensionChecker =>

  def runEvaluator(tree: ASTNode): ASTNode = messageScope(phase = 'evaluator) {
    rewrite(reduce(select + substitute + removeShares)) (tree)
  }
  
  // Our reduction strategy
  // top down traversal breaks attribution links to parents, so lookup of bindings cannot be performed
  private[phases] def reduce(s: Strategy): Strategy = repeat(oncebu(s))

}