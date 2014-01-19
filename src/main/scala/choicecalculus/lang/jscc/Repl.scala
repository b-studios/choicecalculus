package choicecalculus
package lang.jscc

import phases.generator.DefaultGenerator

object Repl extends choicecalculus.Repl with DefaultGenerator with JsCcParser {

  val prettyprinter = PrettyPrinter

}