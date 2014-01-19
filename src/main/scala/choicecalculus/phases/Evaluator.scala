package choicecalculus
package phases

import evaluator.{ Selection, Substitution }

import org.kiama.rewriting.{ Rewriter, Strategy }

import lang.trees.Tree

import utility.messages._

/**
 * <h2> The Evaluator phase
 */
trait Evaluator extends Selection with Substitution {
  self: Reader with Namer with DimensionChecker with Rewriter =>

  def runEvaluator(tree: Tree): Tree = messageScope(phase = 'evaluator) {
    rewrite(reductionStrategy(select + substitute + removeShares))(tree)
  }

  // Our reduction strategy
  // top down traversal breaks attribution links to parents, so lookup of bindings cannot be performed
  private[phases] def reductionStrategy(s: Strategy): Strategy = repeat(oncebu(s))

}