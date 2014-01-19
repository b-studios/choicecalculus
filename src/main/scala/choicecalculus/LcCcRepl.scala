package choicecalculus

import phases.generator.DefaultGenerator

object LcCcRepl extends Repl with DefaultGenerator with lang.lccc.LcCcParser {

  val prettyprinter = lang.lccc.PrettyPrinter

}