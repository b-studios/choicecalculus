package choicecalculus
package lang
package jscc

import utility.messages._
import trees._

/**
 * Implements the interface required by [[phases.Parser]]
 */
trait JsCcParser extends phases.Parser {

  object parsers extends ParsersAPI with Parser {

    def parseFile(p: TreeParser, in: java.io.Reader): Tree = {

      // Do not wrap topLevel into strippedPhrase!
      val parser: TreeParser = if (p == topLevel) p else strippedPhrase(p)

      parseAll(parser, in) match {
        case Success(ast, _) => ast
        case f => raise(f.toString, phase = 'parser)
      }
    }
  }
}

trait Parser extends javascript.Parser {

  lazy val parser: PackratParser[Tree] = topLevel

  private lazy val cc_name: PackratParser[Symbol] = """[a-zA-Z$_][a-zA-Z0-9$_]*""".r ^^ (Symbol(_))

  private lazy val cc_string: PackratParser[String] = '"' ~> consumed((not('"') ~ any).*) <~ '"'

  private val cc_prefix = "#"

  private lazy val typeParser: PackratParser[PackratParser[Tree]] =
    ( "Expression" ^^^ assignExpr
    | "Statement"  ^^^ statement
    | "Literal"    ^^^ literal
    )

  /**
   * cc_node is a parser for all kinds of choice calculus nodes, parametrized over
   * their body parser. This body parser is dependend on the context in which the cc-node
   * occurs.
   */
  private def cc_node(body: PackratParser[Tree]): PackratParser[Tree] =
    ( cc_dimension(body)
    | cc_choice(body)
    | cc_select(body)
    | cc_share(body)
    | cc_include(body)
    | cc_identifier(body)
    )

  private def cc_dimension(body: PackratParser[Tree]): PackratParser[Tree] =
    ('dim ␣> cc_name) ␣ ("(" ␣> listOf(cc_name, ",") <␣ ")" ␣ 'in.? ) ␣ body ^^ {
      case dim ~ tags ~ body => Dimension(dim, tags, body)
    }

  private def cc_choice(body: PackratParser[Tree]): PackratParser[Tree] =
    ('choice ␣> cc_name) ␣ ("{" ␣> multiple(memo(cc_alternative(body))) <␣ "}") ^^ {
      case dim ~ alternatives => Choice(dim, alternatives)
    }

  private def cc_alternative(body: PackratParser[Tree]): PackratParser[Alternative] =
    ('case ␣> cc_name <␣ ("=>" | "→")) ␣ body ^^ Alternative


  private def cc_select(body: PackratParser[Tree]): PackratParser[Tree] =
    'select ␣> (cc_name ~ ("." ~> cc_name)) ␣ ('from.? ␣> body) ^^ {
      case dim ~ tag ~ body => Select(dim, tag, body)
    }

  private def cc_share(body: PackratParser[Tree]): PackratParser[Tree] =
    ('share ␣> cc_prefix ~> cc_name <␣ ":") ␣ (typeParser into { (spaces ~ 'as) ␣> _ <␣ 'within }) ␣ body ^^ {
      case id ~ binding ~ body => Share(id, binding, body)
    }

  private def cc_include(body: PackratParser[Tree]): PackratParser[Tree] =
    'include ␣> cc_string ^^ {
      case filename => Include(filename, body)
    }

  private def cc_identifier(body: PackratParser[Tree]): PackratParser[Tree] =
    cc_prefix ~> cc_name ^^ {
      case name => Identifier(name)
    }

  private lazy val cc_expression = cc_node(expression)
  private lazy val cc_statement  = cc_node(statement)

  // wiring of cc parser and hostlanguage parser

  override def _primExpr =
    ( cc_expression
    | super._primExpr
    )

  override def _statement =
    ( cc_statement
    | super._statement
    )

  // putting `expression` instead of `idLiteral` as argument to cc_expression
  // eliminates the need of wrapping the body of expressions multiple times
  // TODO open bug for that
  override def _idLiteral =
    ( "[[" ~> cc_expression <~ "]]"
    | super._idLiteral
    )
}