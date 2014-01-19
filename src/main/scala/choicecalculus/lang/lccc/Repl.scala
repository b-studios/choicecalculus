package choicecalculus
package lang.lccc

import phases.generator.DefaultGenerator

object Repl extends choicecalculus.Repl with DefaultGenerator with LcCcParser {

  val prettyprinter = PrettyPrinter

}