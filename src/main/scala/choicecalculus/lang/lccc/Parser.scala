package choicecalculus
package lang.lccc

import trees._
import lang.trees._
import utility.messages._

trait LcCcParser extends phases.Parser {

  object parsers extends ParsersAPI with Parser {

    def parseFile(p: TreeParser, in: java.io.Reader): Tree = {

      parseAll(strippedPhrase(p), in) match {
        case Success(ast, _) => ast
        case f => raise(f.toString, phase = 'parser)
      }
    }
  }
}

/**
 * We reuse the javascript lexer here for simplicity
 */
trait Parser extends lang.javascript.Lexer {

  // The Lexer
  lazy val nameSym: PackratParser[Symbol] = 
    name ^^ { case n => Symbol(n) }

  lazy val strippedString: PackratParser[String] = 
    '"' ~> consumed((not('"') ~ any).*) <~ '"'


  // The Parser
  lazy val topLevel = lcExpression


  // Lambda Calculus
  lazy val lcExpression: PackratParser[Tree] = lambdaExpr

  // We allow lcExpression in parameter position to enable variational names
  lazy val lambdaExpr: PackratParser[Tree] = 
    ( ("\\" ~> lcExpression <␣ ".") ␣ lambdaExpr ^^ Lambda
    | binaryOpExpr
    )

  lazy val binaryOpExpr: PackratParser[Tree] = 
    ( binaryOpExpr ␣ ("+" | "-") ␣ callExpr ^^ BinaryOp
    | callExpr
    )

  lazy val callExpr: PackratParser[Tree] = 
    ( callExpr ␣ unaryExpr ^^ App
    | unaryExpr
    )

  lazy val unaryExpr: PackratParser[Tree] = 
    ( ccExpression
    | literal
    | "(" ␣> lcExpression <␣ ")" ^^ Grouping
    )

  lazy val literal: PackratParser[Tree] = 
    ( idLiteral
    | (string | number) ^^ Literal
    ) 

  lazy val idLiteral: PackratParser[Tree] = 
    name ^^ Literal


  // Choice Calculus
  lazy val ccExpression: PackratParser[Tree] = 
    ( dimensionExpr
    | selectExpr
    | shareExpr
    | includeExpr
    | ccIdentifier
    | choiceExpr
    )

  lazy val dimensionExpr: PackratParser[Tree] = 
    'dim ␣> nameSym ␣ ("<" ␣> listOf(nameSym, ",") <␣ ">") ␣ ('in ␣> lcExpression) ^^ Dimension

  lazy val selectExpr: PackratParser[Tree] = 
    'select ␣> (nameSym <~ ".") ~ (nameSym <␣ 'from) ␣ lcExpression ^^ Select

  lazy val shareExpr: PackratParser[Tree] = 
    'share ␣> ("#" ~> nameSym <␣ "=") ␣ lcExpression ␣ ('in ␣> lcExpression) ^^ Share

  lazy val includeExpr: PackratParser[Tree] =
    'include ␣> strippedString ^^ {
      case filename => Include(filename, lcExpression)
    }

  lazy val ccIdentifier: PackratParser[Tree] = 
    "#" ~> nameSym ^^ Identifier

  lazy val choiceExpr: PackratParser[Tree] =
    nameSym ~ ("<" ␣> listOf(alternative, ",") <␣ ">") ^^ Choice

  lazy val alternative: PackratParser[Alternative] =
    nameSym ␣ (":" ␣> lcExpression ) ^^ Alternative
}