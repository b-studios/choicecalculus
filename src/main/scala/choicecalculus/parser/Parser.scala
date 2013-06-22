package choicecalculus
package parser

import org.kiama.util.PositionedParserUtilities
import utility.ParserUtils
import ast.{ ASTNode, Expression, Statement, IncludeExpr }

trait HostLanguageParser extends PositionedParserUtilities with ParserUtils {  
  
  val topLevel: PackratParser[ASTNode]
  val typeParser: PackratParser[PackratParser[ASTNode]]
  
  
  // using this way of wiring, since overriding definitions does not work
  // well with PackratParser's `lazy val`
  //
  // would be cool to have a more generic way of doing this
  val expression: PackratParser[Expression]
  val expressionHook: PackratParser[Expression]
  val statement: PackratParser[Statement]
  val statementHook: PackratParser[Statement]
}


trait Parser extends JavaScriptParser {
  
  import ast.{ASTNode, CCExpression, DimensionExpr, ChoiceExpr, Choice, SelectExpr, IdExpr, ShareExpr }
  
  lazy val parser: PackratParser[ASTNode] =
    phrase (topLevel).named("toplevelphrase")      
      
  
  lazy val cc_id: PackratParser[Symbol]        = """[a-zA-Z$_][a-zA-Z0-9$_]*""".r ^^ (Symbol(_))
  
  lazy val cc_string: PackratParser[String]    = '"' ~> consumed ((not('"') ~ any).*) <~ '"'

  lazy val expressionHook: PackratParser[Expression] = cc_expression(expression)
  lazy val statementHook: PackratParser[Statement] = cc_expression(statement)
  
  val cc_prefix = "#"
  
  /**
   * cc_expression is a parser for all kinds of choice calculus expressions, parametrized over
   * their body parser. This body parser is dependend on the context in which the cc-expression
   * occurs.
   */
  def cc_expression[T <: ASTNode](body: PackratParser[T]): PackratParser[T] = 
    memo( cc_dimensionExpr(body)
        | cc_choiceExpr(body)
        | cc_selectExpr(body)
        | cc_shareExpr(body)
        | cc_includeExpr[T](body)
        | cc_idExpr[T](body)    
        | failure ("Expected choice calculus expression")
        )
    
  private def cc_dimensionExpr[T <: ASTNode](body: PackratParser[T]): PackratParser[T] =
      
    // This cast can only succeed, if CCExpression is subtype of T
    ("dim" ␣> cc_id) ␣ ("(" ␣> listOf(cc_id, ",") <␣ ")" ␣ "in".? ) ␣ body ^^ {
      case dim ~ tags ~ body => DimensionExpr(dim, tags, body).asInstanceOf[T]
    }     
      
  private def cc_choiceExpr[T <: ASTNode](body: PackratParser[T]): PackratParser[T] =
    ( ("choice" ␣> cc_id) ␣ ("{" ␣> multiple(memo(cc_choice(body))) <␣ "}") ^^ {
        case dim ~ choices => ChoiceExpr(dim, choices).asInstanceOf[T] 
      }
    /*| cc_id ␣ ("«" ␣> listOf(cc_inlineChoice, ",") <␣ "»") ^^ {
        case dim ~ choices => ChoiceExpr(dim, choices).asInstanceOf[T] 
      }*/
    )
    
  private def cc_choice[T <: ASTNode](body: PackratParser[T]): PackratParser[Choice[T]] =
    ("case" ␣> cc_id <␣ ("=>" | "→")) ␣ body ^^ Choice[T]
  
  // ugly hack
  // JavaScript's comma operator causes some trouble here, since the comma in A<a:1,b:2> is parsed as comma operator
  lazy val cc_inlineChoice: PackratParser[Choice[Expression]] =
    (cc_id <␣ ":") ␣ assignExpr ^^ Choice[Expression]
      
  private def cc_selectExpr[T <: ASTNode](body: PackratParser[T]): PackratParser[T] =
    "select" ␣> (cc_id ~ ("." ~> cc_id)) ␣ ("from".? ␣> body) ^^ {
      case dim ~ tag ~ body => SelectExpr(dim, tag, body).asInstanceOf[T]
    }
      
  private def cc_shareExpr[T <: ASTNode](body: PackratParser[T]): PackratParser[T] =
    ("share" ␣> cc_prefix ~> cc_id <␣ ":") ␣ (typeParser into { (spaces ~ "as") ␣>  _ <␣  "within" }) ␣ body ^^ {
      case id ~ binding ~ body => ShareExpr(id, binding, body).asInstanceOf[T]
    }
    
  private def cc_includeExpr[T <: ASTNode](body: PackratParser[T]): PackratParser[T] =
    "include" ␣> cc_string ^^ {
      case filename =>{
        println(body)
        IncludeExpr(filename, body).asInstanceOf[T]
      } 
    }
      
 private def cc_idExpr[T <: ASTNode](body: PackratParser[T]): PackratParser[T] =
   cc_prefix ~> cc_id ^^ { 
     case name => IdExpr(name).asInstanceOf[T]
   }
}