package choicecalculus.lang

import trees.Tree
import org.kiama.output

trait PrettyPrinter extends output.PrettyPrinter {

  def toDoc(e: Tree): Doc = sys error "Implement toDoc"

  implicit class PrettyPrinterOps(that: Tree) {
    def pretty(implicit pp: PrettyPrinter): String = pp.pretty(pp.toDoc(that))
  }
}