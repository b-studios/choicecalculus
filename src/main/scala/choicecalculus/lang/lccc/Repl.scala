package choicecalculus
package lang.lccc

import phases.generator.DefaultGenerator

object LcCcRepl extends choicecalculus.Repl with DefaultGenerator with LcCcParser {

  val prettyprinter = PrettyPrinter

}