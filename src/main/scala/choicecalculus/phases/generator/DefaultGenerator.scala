package choicecalculus
package phases
package generator

import lang.trees.Tree
import lang.PrettyPrinter

trait DefaultGenerator extends Generator {

  val prettyprinter: PrettyPrinter

  def runGenerator(tree: Tree): String = 
    prettyprinter.pretty(prettyprinter.toDoc(tree))

}