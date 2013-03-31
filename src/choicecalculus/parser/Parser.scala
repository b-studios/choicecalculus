package choicecalculus
package parser

import org.kiama.util.PositionedParserUtilities
import utility.ParserUtils
import ast.ASTNode

trait HostLanguageParser extends PositionedParserUtilities with ParserUtils {
  
  val topLevel: PackratParser[ASTNode]
  
}

// Maybe we have to introduce the CC keywords as additional reserved keywords
trait ChoiceCalculusParser { self: HostLanguageParser =>
  
  import ast.{ASTNode, CCExpression, DimensionExpr, ChoiceExpr, Choice, SelectExpr, IdExpr, ShareExpr}
  
  
  
  lazy val cc_id: PackratParser[Symbol]        = """[a-zA-Z$_][a-zA-Z0-9$_]*""".r ^^ (Symbol(_))
  
  
   def cc_expression[T <: ASTNode](body: PackratParser[T]): PackratParser[T] = 
      ( cc_dimensionExpr(body)
      | cc_choiceExpr(body)
      | cc_selectExpr(body)
      | cc_shareExpr(body)
      | cc_idExpr[T]
      | body
      | failure ("Expected choice calculus expression")
      )      
    
    def cc_dimensionExpr[T <: ASTNode](body: PackratParser[T]): PackratParser[T] =
      
      // This cast can only succeed, if CCExpression is subtype of T
      ("dim" ␣> cc_id) ␣ ("<" ␣> listOf(cc_id, ",") <␣ ">") ␣ body ^^ {
        case dim ~ tags ~ body => DimensionExpr(dim, tags, body).asInstanceOf[T]
      }
      
      
      
    def cc_choiceExpr[T <: ASTNode](body: PackratParser[T]): PackratParser[T] =
      ( ("choice" ␣> cc_id) ␣ ("{" ␣> multiple(cc_choice(body)) <␣ "}") ^^ {
          case dim ~ choices => ChoiceExpr(dim, choices).asInstanceOf[T] 
        }
      | cc_id ␣ ("<" ␣> listOf(cc_inlineChoice(body), ",") <␣ ">") ^^ {
          case dim ~ choices => ChoiceExpr(dim, choices).asInstanceOf[T] 
        }
      )
    
    def cc_choice[T <: ASTNode](body: PackratParser[T]): PackratParser[Choice] =
      ("case" ␣> cc_id <␣ "=>") ␣ body ^^ Choice
    
    def cc_inlineChoice[T <: ASTNode](body: PackratParser[T]): PackratParser[Choice] =
      (cc_id <␣ ":") ␣ body ^^ Choice
      
    def cc_selectExpr[T <: ASTNode](body: PackratParser[T]): PackratParser[T] =
      "select" ␣> (cc_id ~ ("." ~> cc_id)) ␣ ("from" ␣> body) ^^ {
         case dim ~ tag ~ body => SelectExpr(dim, tag, body).asInstanceOf[T]
        }
      
    def cc_shareExpr[T <: ASTNode](body: Parser[T]): PackratParser[T] =
      "share" ␣> "§" ~> cc_id ␣ ("=" ␣> body) ␣ body ^^ {
         case id ~ binding ~ body => ShareExpr(id, binding, body).asInstanceOf[T]
        }
      
    // TODO Problem, how to handle the static type of an ID???
    // Here disambiguation has to take place, if the host language uses ids
    def cc_idExpr[T <: ASTNode]: PackratParser[T] =
      "§" ~> cc_id ^^ { 
        case name => IdExpr(name).asInstanceOf[T]
      }
      
}


trait Parser extends JavaScriptParser with ChoiceCalculusParser {
  
    import ast._
    
    override def keywords = super.keywords //++ Set('share, 'select, 'case, 'in, 'choice)
    
    lazy val parser: PackratParser[ASTNode] =
      phrase (topLevel)
      
    // Do some wiring
    override def declaration = 
      cc_expression[Statement](super.declaration)          
      
    override def statement = 
      cc_expression[Statement](super.statement)
      
    override def expression = 
      cc_expression[Expression](super.expression)
            
}