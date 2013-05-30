package choicecalculus
package parser

import org.kiama.util.PositionedParserUtilities
import utility.ParserUtils
import ast.ASTNode
import choicecalculus.ast.IncludeExpr

trait HostLanguageParser extends PositionedParserUtilities with ParserUtils {
  
  val topLevel: PackratParser[ASTNode]
  val typeParser: PackratParser[PackratParser[ASTNode]]
  
}

// Maybe we have to introduce the CC keywords as additional reserved keywords
trait ChoiceCalculusParser { self: HostLanguageParser =>
  
  import ast.{ASTNode, CCExpression, DimensionExpr, ChoiceExpr, Choice, SelectExpr, IdExpr, ShareExpr}
  
  
  
  lazy val cc_id: PackratParser[Symbol]        = """[a-zA-Z$_][a-zA-Z0-9$_]*""".r ^^ (Symbol(_))
  
  lazy val cc_string: PackratParser[String]    = '"' ~> consumed ((not('"') ~ any).*) <~ '"'
  
  
  
  /**
   * cc_expression is a parser for all kinds of choice calculus expressions, parametrized over
   * their body parser. This body parser is dependend on the context in which the cc-expression
   * occurs.
   */
  def cc_expression[T <: ASTNode](body: PackratParser[T]): PackratParser[T] = 
    ( cc_dimensionExpr(body)
    | cc_choiceExpr(body)
    | cc_selectExpr(body)
    | cc_shareExpr(body)
    | cc_includeExpr[T](body)
    | cc_idExpr[T](body)
    | body
    | failure ("Expected choice calculus expression")
    )
    
  def cc_dimensionExpr[T <: ASTNode](body: PackratParser[T]): PackratParser[T] =
      
    // This cast can only succeed, if CCExpression is subtype of T
    ("dim" ␣> cc_id) ␣ ("<" ␣> listOf(cc_id, ",") <␣ ">" ␣ "in".?) ␣ body ^^ {
      case dim ~ tags ~ body => DimensionExpr(dim, tags, body).asInstanceOf[T]
    }
      
      
      
    def cc_choiceExpr[T <: ASTNode](body: PackratParser[T]): PackratParser[T] =
      ( ("choice" ␣> cc_id) ␣ ("{" ␣> multiple(cc_choice(body)) <␣ "}") ^^ {
          case dim ~ choices => ChoiceExpr(dim, choices).asInstanceOf[T] 
        }
      
      // JavaScript's comma operator causes some trouble here, since the comma in A<a:1,b:2> is parsed as comma operator
      | cc_id ␣ ("<" ␣> listOf(cc_inlineChoice(body), ",") <␣ ">") ^^ {
          case dim ~ choices => ChoiceExpr(dim, choices).asInstanceOf[T] 
        }
      )
    
    def cc_choice[T <: ASTNode](body: PackratParser[T]): PackratParser[Choice[T]] =
      ("case" ␣> cc_id <␣ "=>") ␣ body ^^ Choice[T]
    
    def cc_inlineChoice[T <: ASTNode](body: PackratParser[T]): PackratParser[Choice[T]] =
      (cc_id <␣ ":") ␣ body ^^ Choice[T]
      
    def cc_selectExpr[T <: ASTNode](body: PackratParser[T]): PackratParser[T] =
      "select" ␣> (cc_id ~ ("." ~> cc_id)) ␣ ("from" ␣> body) ^^ {
         case dim ~ tag ~ body => SelectExpr(dim, tag, body).asInstanceOf[T]
        }
      
    def cc_shareExpr[T <: ASTNode](body: PackratParser[T]): PackratParser[T] =
      ("share" ␣> "§" ~> cc_id <␣ ":") ␣ (typeParser into { (spaces ~ "=") ␣> _ <␣  "in" }) ␣ body ^^ {
         case id ~ binding ~ body => ShareExpr(id, binding, body).asInstanceOf[T]
        }
    
    def cc_includeExpr[T <: ASTNode](body: PackratParser[T]): PackratParser[T] =
      "include" ␣> cc_string ^^ {
        case filename =>{
          println(body)
          IncludeExpr(filename, body).asInstanceOf[T]
        } 
      }
      
    // TODO Problem, how to handle the static type of an ID???
    // Here disambiguation has to take place, if the host language uses ids
    def cc_idExpr[T <: ASTNode](body: PackratParser[T]): PackratParser[T] =
      "§" ~> cc_id ^^ { 
        case name => IdExpr(name).asInstanceOf[T]
      }
      
}


trait Parser extends JavaScriptParser with ChoiceCalculusParser {
  
    import ast._
    
    override def keywords = super.keywords //++ Set('share, 'select, 'case, 'in, 'choice)
    
    lazy val parser: PackratParser[ASTNode] =
      phrase (topLevel).named("toplevelphrase")      
      
    // Do some wiring
    override def declaration = 
      cc_expression[Statement](super.declaration.named("hostlanguageDeclaration"))       
      
    override def statement = 
      cc_expression[Statement](super.statement.named("hostlanguageStatement"))
      
    override def expression = 
      cc_expression[Expression](super.expression.named("hostlanguageExpression"))

    override def assignExpr = 
      cc_expression[Expression](super.assignExpr.named("hostlanguageAssignExpr"))
            
}