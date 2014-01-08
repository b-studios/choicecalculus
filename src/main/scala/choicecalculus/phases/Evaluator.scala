package choicecalculus
package phases

import evaluator.{ Selection, Substitution }
import lang.ASTNode

import utility.DebugRewriter.{ oncebu, rewrite, repeat }

import org.kiama.rewriting.Strategy


trait Evaluator extends Selection with Substitution { 
  self: Reader with Namer with DimensionChecker =>

  def runEvaluator(tree: ASTNode): ASTNode =
    rewrite(reduce(select + substitute + removeShares)) (tree)

  // Our reduction strategy
  // top down traversal breaks attribution links to parents, so lookup of bindings cannot be performed
  private def reduce(s: Strategy): Strategy = repeat(oncebu(s))
  
}