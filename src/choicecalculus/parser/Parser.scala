package choicecalculus
package parser

import org.kiama.util.PositionedParserUtilities

/*
trait ChoiceCalculusParser extends PositionedParserUtilities {
  
  import ast.{ASTNode, CCExpression, DimensionExpr, ChoiceExpr, Choice, SelectExpr, IdExpr}
  
  lazy val ID: PackratParser[Symbol]        = """[a-zA-Z$_][a-zA-Z0-9$_]*""".r ^^ (Symbol(_))
  
  
  
   def choiceCalculusExpr(body: Parser[ASTNode]): PackratParser[CCExpression] = 
      ( dimensionExpr(body)
      | choiceExpr(body)
      | selectExpr(body)
      | shareExpr(body)
      | idExpr
      )      
      
    
    def dimensionExpr(body: Parser[ASTNode]): PackratParser[CCExpression] =
      ("dim" ~> ID) ~ ("<" ~> rep1sep(ID, ",") <~ ">") ~ ("in" ~> body) ^^ DimensionExpr
    
    def choiceExpr(body: Parser[ASTNode]): PackratParser[CCExpression] =
      ( ("choice" ~> ID) ~ ("{" ~> rep1(choice(body)) <~ "}") ^^ ChoiceExpr
      | ID ~ ("<" ~> rep1sep(inlinechoice(body), ",") <~ ">") ^^ ChoiceExpr
      )
    
    def choice(body: Parser[ASTNode]): PackratParser[Choice] =
      ("case" ~> ID <~ "=>") ~ body ^^ Choice
    
    def inlinechoice(body: Parser[ASTNode]): PackratParser[Choice] =
      (ID <~ ":") ~ body ^^ Choice
      
    def selectExpr(body: Parser[ASTNode]): PackratParser[CCExpression] =
      ("select" ~> (ID ~ ("." ~> ID)) <~ "from") ~ body ^^ SelectExpr
      
    def shareExpr(body: Parser[ASTNode]): PackratParser[CCExpression] =
      ("share" ~> (ID ~ ("=" ~> body <~ "in"))) ~ body ^^ ShareExpr
    
    // TODO Problem, how to handle the static type of an ID???
    // Here disambiguation has to take place, if the host language uses ids
    lazy val idExpr: PackratParser[CCExpression] =
      ID ^^ IdExpr
      
}
*/

trait Parser extends JavaScriptParser {
  
    import ast._
    
    lazy val parser: PackratParser[ASTNode] =
      phrase (topLevel)
          
}