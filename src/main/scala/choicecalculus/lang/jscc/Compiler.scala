package choicecalculus
package lang.jscc

import phases.generator.DefaultGenerator

object Compiler extends choicecalculus.Compiler with DefaultGenerator with JsCcParser {

  val prettyprinter = PrettyPrinter

}