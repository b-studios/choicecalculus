package choicecalculus.lang
package choicecalculus

import javascript.JavaScriptParser

trait ChoiceCalculusParser extends JavaScriptParser {
  
  lazy val parser: PackratParser[ASTNode]      = topLevel
  
  lazy val cc_id: PackratParser[Symbol]        = """[a-zA-Z$_][a-zA-Z0-9$_]*""".r ^^ (Symbol(_))
  
  lazy val cc_string: PackratParser[String]    = '"' ~> consumed ((not('"') ~ any).*) <~ '"'

  val cc_prefix = "#"
  
  lazy val typeParser: PackratParser[PackratParser[ASTNode]] =
    ( "Expression" ^^^ assignExpr
    | "Statement"  ^^^ statement
    | "Literal"    ^^^ literal
    )
    
  /**
   * cc_expression is a parser for all kinds of choice calculus expressions, parametrized over
   * their body parser. This body parser is dependend on the context in which the cc-expression
   * occurs.
   */
  def cc_expression[T <: ASTNode](body: PackratParser[T]): PackratParser[ChoiceCalculusNode] = 
    ( cc_dimensionExpr(body)
    | cc_choiceExpr(body)
    | cc_selectExpr(body)
    | cc_shareExpr(body)
    | cc_includeExpr[T](body)
    | cc_idExpr[T](body)
    )

  private def cc_dimensionExpr[T <: ASTNode](body: PackratParser[T]): PackratParser[ChoiceCalculusNode] =
    ('dim ␣> cc_id) ␣ ("(" ␣> listOf(cc_id, ",") <␣ ")" ␣ 'in.? ) ␣ body ^^ {
      case dim ~ tags ~ body => Dimension(dim, tags, body)
    }     
      
  private def cc_choiceExpr[T <: ASTNode](body: PackratParser[T]): PackratParser[ChoiceCalculusNode] =
    ( ('choice ␣> cc_id) ␣ ("{" ␣> multiple(memo(cc_choice(body))) <␣ "}") ^^ {
        case dim ~ choices => Choices(dim, choices)
      }
    )
    
  private def cc_choice[T <: ASTNode](body: PackratParser[T]): PackratParser[Choice[T]] =
    ('case ␣> cc_id <␣ ("=>" | "→")) ␣ body ^^ Choice[T]
 
      
  private def cc_selectExpr[T <: ASTNode](body: PackratParser[T]): PackratParser[ChoiceCalculusNode] =
    'select ␣> (cc_id ~ ("." ~> cc_id)) ␣ ('from.? ␣> body) ^^ {
      case dim ~ tag ~ body => Select(dim, tag, body)
    }
      
  private def cc_shareExpr[T <: ASTNode](body: PackratParser[T]): PackratParser[ChoiceCalculusNode] =
    ('share ␣> cc_prefix ~> cc_id <␣ ":") ␣ (typeParser into { (spaces ~ 'as) ␣>  _ <␣  'within }) ␣ body ^^ {
      case id ~ binding ~ body => Share(id, binding, body)
    }
    
  private def cc_includeExpr[T <: ASTNode](body: PackratParser[T]): PackratParser[ChoiceCalculusNode] =
    'include ␣> cc_string ^^ {
      case filename => Include(filename, body)
    }
      
  private def cc_idExpr[T <: ASTNode](body: PackratParser[T]): PackratParser[ChoiceCalculusNode] =
    cc_prefix ~> cc_id ^^ { 
      case name => SharedId(name)
    }
   
  // wiring of cc parser and hostlanguage parser
  override def _expression = 
    ( cc_expression(expression)
    | super._expression
    )
    
  override def _assignExpr = 
    ( cc_expression(expression)
    | super._assignExpr
    )
  
  override def _primExpr = 
    ( cc_expression(expression)
    | super._primExpr
    )
    
  override def _statement = 
    ( cc_expression(statement)
    | super._statement
    )
    
  // putting `expression` instead of `idLiteral` as argument to cc_expression
  // eliminates the need of wrapping the body of expressions multiple times
  // TODO open bug for that
  override def _idLiteral =
    ( "[[" ~> cc_expression(expression) <~ "]]"
    | super._idLiteral
    )
}