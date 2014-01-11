package choicecalculus.lang
package choicecalculus

import javascript.JavaScriptParser

trait ChoiceCalculusParser extends JavaScriptParser {

  lazy val parser: PackratParser[ASTNode] = topLevel

  lazy val cc_name: PackratParser[Symbol] = """[a-zA-Z$_][a-zA-Z0-9$_]*""".r ^^ (Symbol(_))

  lazy val cc_string: PackratParser[String] = '"' ~> consumed((not('"') ~ any).*) <~ '"'

  val cc_prefix = "#"

  lazy val typeParser: PackratParser[PackratParser[ASTNode]] =
    ( "Expression" ^^^ assignExpr
    | "Statement"  ^^^ statement
    | "Literal"    ^^^ literal
    )

  /**
   * cc_node is a parser for all kinds of choice calculus nodes, parametrized over
   * their body parser. This body parser is dependend on the context in which the cc-node
   * occurs.
   */
  def cc_node[T <: ASTNode](body: PackratParser[T]): PackratParser[ChoiceCalculusNode] =
    ( cc_dimension(body)
    | cc_choice(body)
    | cc_select(body)
    | cc_share(body)
    | cc_include[T](body)
    | cc_identifier[T](body)
    )

  private def cc_dimension[T <: ASTNode](body: PackratParser[T]): PackratParser[ChoiceCalculusNode] =
    ('dim ␣> cc_name) ␣ ("(" ␣> listOf(cc_name, ",") <␣ ")" ␣ 'in.? ) ␣ body ^^ {
      case dim ~ tags ~ body => Dimension(dim, tags, body)
    }

  private def cc_choice[T <: ASTNode](body: PackratParser[T]): PackratParser[ChoiceCalculusNode] =
    ('choice ␣> cc_name) ␣ ("{" ␣> multiple(memo(cc_alternative(body))) <␣ "}") ^^ {
      case dim ~ alternatives => Choice(dim, alternatives)
    }

  private def cc_alternative[T <: ASTNode](body: PackratParser[T]): PackratParser[Alternative[T]] =
    ('case ␣> cc_name <␣ ("=>" | "→")) ␣ body ^^ Alternative[T]


  private def cc_select[T <: ASTNode](body: PackratParser[T]): PackratParser[ChoiceCalculusNode] =
    'select ␣> (cc_name ~ ("." ~> cc_name)) ␣ ('from.? ␣> body) ^^ {
      case dim ~ tag ~ body => Select(dim, tag, body)
    }

  private def cc_share[T <: ASTNode](body: PackratParser[T]): PackratParser[ChoiceCalculusNode] =
    ('share ␣> cc_prefix ~> cc_name <␣ ":") ␣ (typeParser into { (spaces ~ 'as) ␣> _ <␣ 'within }) ␣ body ^^ {
      case id ~ binding ~ body => Share(id, binding, body)
    }

  private def cc_include[T <: ASTNode](body: PackratParser[T]): PackratParser[ChoiceCalculusNode] =
    'include ␣> cc_string ^^ {
      case filename => Include(filename, body)
    }

  private def cc_identifier[T <: ASTNode](body: PackratParser[T]): PackratParser[ChoiceCalculusNode] =
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