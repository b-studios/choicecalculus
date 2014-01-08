package choicecalculus

import lang.ASTNode
import lang.choicecalculus.ChoiceCalculusParser

object JsCcCompiler extends Compiler {

  object parsers extends ChoiceCalculusParser with ParsersAPI {
    def parseFile(p: TreeParser, filename: String, in: java.io.Reader): ASTNode = 
      parseAll(strippedPhrase(p), in) match {
        case Success(ast,_) => ast
        case f => sys error s"Failure while parsing $filename\n${f.toString}"
      }
  }

}