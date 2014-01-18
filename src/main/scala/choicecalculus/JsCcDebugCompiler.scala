package choicecalculus

import phases.generator.DebugGenerator

import lang.trees.Tree

object JsCcDebugCompiler extends Compiler with DebugGenerator with lang.jscc.JsCcParser {

  override def runEvaluator(tree: Tree): Tree = tree

  object prettyprinter extends lang.jscc.PrettyPrinter with DebugPrettyPrinter

}