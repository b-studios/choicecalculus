package choicecalculus
package parser

import org.kiama.util.PositionedParserUtilities
import utility.ParserUtils
import ast.ASTNode
import choicecalculus.ast.IncludeExpr

trait HostLanguageParser extends PositionedParserUtilities with ParserUtils {
  
  val topLevel: PackratParser[ASTNode]
  
}

// Maybe we have to introduce the CC keywords as additional reserved keywords
trait ChoiceCalculusParser { self: HostLanguageParser =>
  
  import ast.{ASTNode, CCExpression, DimensionExpr, ChoiceExpr, Choice, SelectExpr, IdExpr, ShareExpr}
  
  import ast.{ Builder }
  
  
  lazy val cc_id: PackratParser[Symbol]        = """[a-zA-Z$_][a-zA-Z0-9$_]*""".r ^^ (Symbol(_))
  
  lazy val cc_string: PackratParser[String]    = '"' ~> consumed ((not('"') ~ any).*) <~ '"'
      
  def cc_dimensionExpr[T <: ASTNode](body: PackratParser[T])(implicit b: Builder[T]): PackratParser[T] =
      
    // This cast can only succeed, if CCExpression is subtype of T
    ("dim" ␣> cc_id) ␣ ("<" ␣> listOf(cc_id, ",") <␣ ">" ␣ "in".?) ␣ body ^^ {
      case dim ~ tags ~ body => b.dimension(dim, tags, body)
    }
      
      
      
    def cc_choiceExpr[T <: ASTNode](body: PackratParser[T])(implicit b: Builder[T]): PackratParser[T] =
      ( ("choice" ␣> cc_id) ␣ ("{" ␣> multiple(cc_choice(body)) <␣ "}") ^^ {
          case dim ~ choices => b.choice(dim, choices)
        }
      
      // IMPORTANT
      // JavaScript's comma operator causes some trouble here, since the comma in A<a:1,b:2> is parsed as comma operator
      // the problems is also related to `<` being parsed as \leq
      // often the problem is fixed by grouping the choice calculus expression
      // in parenthesis and therefore forcing parsing of an expression.
      //
      // probably we need to override `all` parsers, which generate an expression to
      // first check for CCExpressions.
      /*| cc_id ␣ ("<" ␣> listOf(cc_inlineChoice(body), ",") <␣ ">") ^^ {
          case dim ~ choices => b.choice(dim, choices) 
        }*/
      )
    
    def cc_choice[T <: ASTNode](body: PackratParser[T]): PackratParser[Choice[T]] =
      ("case" ␣> cc_id <␣ "=>") ␣ body ^^ Choice[T]
    
    /*def cc_inlineChoice[T <: ASTNode](body: PackratParser[T])(implicit b: Builder[T]): PackratParser[Choice] =
      (cc_id <␣ ":") ␣ body ^^ Choice
      */
    def cc_selectExpr[T <: ASTNode](body: PackratParser[T])(implicit b: Builder[T]): PackratParser[T] =
      "select" ␣> (cc_id ~ ("." ~> cc_id)) ␣ ("from" ␣> body) ^^ {
         case dim ~ tag ~ body => b.select(dim, tag, body)
        }
     
    // TODO This is wrong - binding has to be parsed with a rule matching the context of the id in use
    def cc_shareExpr[T <: ASTNode](body: Parser[T])(implicit b: Builder[T]): PackratParser[T] =
      "share" ␣> "§" ~> cc_id ␣ ("=" ␣> body) ␣ body ^^ {
         case id ~ binding ~ body => b.share(id, binding, body)
        }
    
    // TODO here body should be passed to parse the file
    def cc_includeExpr[T <: ASTNode](body: Parser[T])(implicit b: Builder[T]): PackratParser[T] =
      "include" ␣> cc_string ^^ {
        case filename => b.include(filename)
      }
      
    // TODO Problem, how to handle the static type of an ID???
    // Here disambiguation has to take place, if the host language uses ids
    def cc_idExpr[T <: ASTNode](body: Parser[T])(implicit b: Builder[T]): PackratParser[T] =
      "§" ~> cc_id ^^ { 
        case name => b.id(name)
      }
      
}


trait Parser extends JavaScriptParser with ChoiceCalculusParser {
  
    import ast._
    
    import builders._
    
    override def keywords = super.keywords //++ Set('share, 'select, 'case, 'in, 'choice)
    
    lazy val parser: PackratParser[ASTNode] =
      phrase (topLevel)
      
    // Do some wiring
   /* override def declaration = 
      cc_expression[Statement](super.declaration)          
      
    override def statement = 
      cc_expression[Statement](super.statement)
     */
      
    override def expression = 
      ( cc_dimensionExpr(super.expression)
      | cc_choiceExpr(super.expression)
      | cc_selectExpr(super.expression)
      | cc_shareExpr(super.expression)
      | cc_includeExpr(super.expression)
      | cc_idExpr(super.expression)
      | super.expression
      )
      
    override def statement = 
      ( cc_dimensionExpr(super.statement)
      | cc_choiceExpr(super.statement)
      | cc_selectExpr(super.statement)
      | cc_shareExpr(super.statement)
      | cc_includeExpr(super.statement)
      | cc_idExpr(super.statement)
      | super.statement
      )  
            
}

object builders {
  
  import ast.{ Builder }
  import ast.{ Expression, Statement, DimensionExpr, ChoiceExpr, Choice, SelectExpr, IdExpr, ShareExpr}
  
  implicit val exprBuilder: Builder[Expression] = new Builder[Expression] {
    
    def dimension(name: Symbol, tags: List[Symbol], body: Expression): Expression = 
      new DimensionExpr[Expression](name, tags, body) with Expression
      
    def choice(dim: Symbol, choices: List[Choice[Expression]]): Expression =
      new ChoiceExpr[Expression](dim, choices) with Expression
      
    def select(dim: Symbol, tag: Symbol, body: Expression): Expression = 
      new SelectExpr[Expression](dim, tag, body) with Expression
      
    def share[S <: ASTNode](name: Symbol, exp: S, body: Expression): Expression =
      new ShareExpr[Expression, S](name, exp, body) with Expression
      
    def id(name: Symbol): Expression = 
      new IdExpr(name) with Expression
      
    def include(filename: String): Expression =
      new IncludeExpr(filename) with Expression
  }
  
  implicit val stmtBuilder: Builder[Statement] = new Builder[Statement] {
    
    def dimension(name: Symbol, tags: List[Symbol], body: Statement): Statement = 
      new DimensionExpr[Statement](name, tags, body) with Statement
      
    def choice(dim: Symbol, choices: List[Choice[Statement]]): Statement =
      new ChoiceExpr[Statement](dim, choices) with Statement
      
    def select(dim: Symbol, tag: Symbol, body: Statement): Statement = 
      new SelectExpr[Statement](dim, tag, body) with Statement
      
    def share[S <: ASTNode](name: Symbol, exp: S, body: Statement): Statement =
      new ShareExpr[Statement, S](name, exp, body) with Statement
      
    def id(name: Symbol): Statement = 
      new IdExpr(name) with Statement
      
    def include(filename: String): Statement =
      new IncludeExpr(filename) with Statement
  }
}