package choicecalculus
package phases

import lang.ASTNode

import scala.util.parsing.combinator.RegexParsers

trait Parser {

  val parsers: ParsersAPI

  trait ParsersAPI { self: RegexParsers => 

    type TreeParser = Parser[ASTNode]

    /**
     * The toplevel parser which is applied in order to parse complete
     * files
     */
    def topLevel: TreeParser

    /**
     * Should apply parser `p` as a phrase, consuming whitespaces
     * before and after applying `p`.
     *
     * Should raise an error if `in` cannot be parsed using the
     * parser `p`. Otherwise return the parsed `ASTNode`.
     *
     * @see scala.util.parsing.combinator.RegexParsers#phrase
     */
    def parseFile(p: TreeParser, name: String, in: java.io.Reader): ASTNode

  }

  /**
   * The type of the parsers returning tree nodes
   */
  type TreeParser = parsers.TreeParser

}