package choicecalculus

import phases.generator.DefaultGenerator

object JsCcRepl extends Repl with DefaultGenerator with lang.jscc.JsCcParser {

  val prettyprinter = lang.jscc.PrettyPrinter

}