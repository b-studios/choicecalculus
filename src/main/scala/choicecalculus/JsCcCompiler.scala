package choicecalculus

import phases.generator.DefaultGenerator

object JsCcCompiler extends Compiler with DefaultGenerator with lang.jscc.JsCcParser {

  val prettyprinter = lang.jscc.PrettyPrinter

}