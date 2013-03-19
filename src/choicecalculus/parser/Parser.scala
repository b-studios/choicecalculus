package choicecalculus
package parser

import org.kiama.util.PositionedParserUtilities

trait Parser extends PositionedParserUtilities {
  
    import ast._
    
    // Lexer Stuff
    lazy val INT: Parser[Int]          = """[0-9]+""".r ^^ (_.toInt)
    lazy val ID: Parser[Symbol]        = """[a-zA-Z$_][a-zA-Z0-9$_]*""".r ^^ (Symbol(_))
    
    lazy val parser: PackratParser[ASTNode] =
      phrase (expression)
    
    
    lazy val expression: PackratParser[Expression] =
      ( choiceCalculusExpr
      | hostLanguageExpression
      )
    
      
    
    // Host Language Expressions
    lazy val hostLanguageExpression: PackratParser[Expression] =      
      addExpr
      
    lazy val addExpr: PackratParser[Expression] =
      ( (addExpr <~ "+") ~ primaryExpr ^^ { 
          case lhs ~ rhs => Add(lhs, rhs)
        }
      | primaryExpr
      )
      
    lazy val primaryExpr: PackratParser[Expression] =
      ( INT ^^ Num
      | "(" ~> expression <~ ")"
      )
      
      
      
    // Choice calculus expression
    lazy val choiceCalculusExpr: PackratParser[CCExpression] =
      ( dimensionExpr
      | choiceExpr
      | selectExpr
      )
      
    
    lazy val dimensionExpr: PackratParser[CCExpression] =
      ("dim" ~> ID) ~ ("<" ~> rep1sep(ID, ",") <~ ">") ~ ("in" ~> expression) ^^ DimensionExpr
    
    lazy val choiceExpr: PackratParser[CCExpression] =
      ( ("choice" ~> ID) ~ ("{" ~> rep1(choice) <~ "}") ^^ ChoiceExpr
      | ID ~ ("<" ~> rep1sep(inlinechoice, ",") <~ ">") ^^ ChoiceExpr
      )
    
    lazy val choice: PackratParser[Choice] =
      ("case" ~> ID <~ "=>") ~ expression ^^ Choice
    
    lazy val inlinechoice: PackratParser[Choice] =
      (ID <~ ":") ~ expression ^^ Choice
      
    lazy val selectExpr: PackratParser[CCExpression] =
      ("select" ~> (ID ~ ("." ~> ID)) <~ "from") ~ expression ^^ SelectExpr
    
}