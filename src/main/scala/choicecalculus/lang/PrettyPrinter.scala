package choicecalculus.lang

import org.kiama.output.ParenPrettyPrinter

trait PrettyPrinter extends ParenPrettyPrinter with org.kiama.output.PrettyPrinter  {
  
  def toDoc(e: ASTNode): Doc = sys error "Implement toDoc"
  
}

object prettyprinter {

  // TODO how can we remove this dependencies in a nice way?
  import choicecalculus.ChoiceCalculusPP
  import javascript.JavaScriptPP

  implicit class PrettyPrinterOps(that: ASTNode) {
    def pretty(implicit pp: PrettyPrinter): String = pp.pretty(pp.toDoc(that))
  }

  // The default pretty printer
  implicit object PrettyPrinter extends JavaScriptPP with ChoiceCalculusPP
}