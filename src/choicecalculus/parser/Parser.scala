package choicecalculus
package parser

import org.kiama.util.PositionedParserUtilities

trait Parser extends PositionedParserUtilities {
  
    import ast._
    
    // Lexer Stuff
    lazy val INT: PackratParser[Int]          = """[0-9]+""".r ^^ (_.toInt)
    lazy val ID: PackratParser[Symbol]        = """[a-zA-Z$_][a-zA-Z0-9$_]*""".r ^^ (Symbol(_))
    
    lazy val parser: PackratParser[ASTNode] =
      phrase (expression)
    
    
    lazy val expression: PackratParser[Expression] =
      hostLanguageExpression    
      
    
    // Host Language Expressions
    lazy val hostLanguageExpression: PackratParser[Expression] =      
      addExpr
      
    lazy val addExpr: PackratParser[Expression] =
      ( (addExpr <~ "+") ~ mulExpr ^^ { 
          case lhs ~ rhs => Add(lhs, rhs)
        }
      | mulExpr
      )
      
    lazy val mulExpr: PackratParser[Expression] =
      ( (mulExpr <~ "*") ~ primaryExpr ^^ { 
          case lhs ~ rhs => Mul(lhs, rhs)
        }
      | primaryExpr
      )
    
    // choice calculusexpr have higher precedence than addExpr
    lazy val primaryExpr: PackratParser[Expression] =
      ( INT ^^ Num
      | "(" ~> expression <~ ")" ^^ GroupExpr
      | functionExpr
      | choiceCalculusExpr
      )      
      
    
    lazy val functionExpr: PackratParser[Expression] = 
      ("function" ~ "(" ~ ")" ~ "{") ~>  expression <~ "}" ^^ FunctionExpr
      
    // Choice calculus expression
    lazy val choiceCalculusExpr: PackratParser[CCExpression] =
      ( dimensionExpr
      | choiceExpr
      | selectExpr
      | shareExpr
      | idExpr
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
      
    lazy val shareExpr: PackratParser[CCExpression] =
      ("share" ~> (ID ~ ("=" ~> expression <~ "in"))) ~ expression ^^ ShareExpr
    
    // Here disambiguation has to take place, if the host language uses ids
    lazy val idExpr: PackratParser[CCExpression] =
      ID ^^ IdExpr
}