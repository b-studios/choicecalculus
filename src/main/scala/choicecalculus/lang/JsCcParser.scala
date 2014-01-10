package choicecalculus
package lang

import choicecalculus.ChoiceCalculusParser

import utility.messages._

trait JsCcParser extends phases.Parser {

  object parsers extends ParsersAPI with ChoiceCalculusParser {

    def parseFile(p: TreeParser, in: java.io.Reader): ASTNode = {

      // Do not wrap topLevel into strippedPhrase!
      val parser: TreeParser = if (p == topLevel) p else strippedPhrase(p)

      parseAll(parser, in) match {
        case Success(ast,_) => ast
        case f => raise(f.toString, phase = 'parser)
      }
    }
  }

}