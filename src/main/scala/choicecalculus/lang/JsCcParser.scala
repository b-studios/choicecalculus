package choicecalculus
package lang

import choicecalculus.ChoiceCalculusParser

trait JsCcParser extends phases.Parser {

  object parsers extends ParsersAPI with ChoiceCalculusParser {

    def parseFile(p: TreeParser, filename: String, in: java.io.Reader): ASTNode = {

      // Do not wrap topLevel into strippedPhrase!
      val parser: TreeParser = if (p == topLevel) p else strippedPhrase(p)

      parseAll(parser, in) match {
        case Success(ast,_) => ast
        case f => sys error s"Failure while parsing $filename\n${f.toString}"
      }
    }
  }

}