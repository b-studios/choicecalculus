package choicecalculus
package lang.jscc

import phases.generator.DebugGenerator

import lang.trees.Tree

object DebugCompiler extends choicecalculus.Compiler with DebugGenerator with JsCcParser {

  override def runEvaluator(tree: Tree): Tree = tree

  object prettyprinter extends PrettyPrinter with DebugPrettyPrinter

}