package choicecalculus
package lang.javascript

import lang.trees.Tree
import lang.javascript.trees._

import org.kiama.util.PositionedParserUtilities
import utility.ParserUtils

trait Lexer extends PositionedParserUtilities with ParserUtils {

  def keywords = Set(
    'break,'case,'catch,'class,'const,'continue,'debugger,
    'default,'delete,'do,'else,'enum,'export,'extends,'for,
    'finally,'function,'if,'import,'in,'instanceof,'new,
    'return,'super,'switch,'this,'throw,'try,'typeof,'var,
    'void,'while,'with
  )

  /**
   * Whitespace Handling
   */
  lazy val whitespace                    = """[\t ]""".r
  lazy val linebreak                     = """(\r\n|\n)""".r
  lazy val singleline                    = """//[^\n]*\n""".r
  lazy val multiline: PackratParser[Any] = "/*" ~ (not ("*/") ~ any).* ~ "*/"
  lazy val eos                           = """\z""".r | failure("Expected end of stream")

  lazy val comment: PackratParser[Any] = singleline | multiline

  lazy val space: PackratParser[Any] = whitespace | linebreak | comment

  lazy val spacesNoNl: PackratParser[Any] = (not (linebreak) ~ space).*

  // "automatic semicolon insertion"
  lazy val sc: PackratParser[Any] =
    spacesNoNl ~ (linebreak | guard("}") | eos | ";")

  /**
   * Identifiers
   */
  lazy val nameFirst  = """[a-zA-Z$_]""".r
  lazy val nameRest   = """[a-zA-Z0-9$_]""".r
  lazy val identifier = "%s(%s)*".format(nameFirst, nameRest).r

  lazy val name: PackratParser[String] =
    consumed(identifier) into {
      case name if keywords contains Symbol(name) => failure("This is a protected keyword!")
      case name => success(name)
    }

  def keyword(name: Symbol): PackratParser[String] =
    consumed(identifier) >> {
      case s if Symbol(s) == name => success(s)
      case s => failure("Not the expected keyword '%s' got '%s'".format(name.name, s))
    }

  /**
   * Numbers
   */
  lazy val digit    = "[0-9]".r
  lazy val hexDigit = "[0-9a-fA-F]".r
  lazy val hex      = "0x" ~ hexDigit.+

  lazy val decimalInt = "(0|[1-9][0-9]*)".r
  lazy val expPart    = """[eE][+\-]?[0-9]+""".r

  lazy val decimal    = "-".? ~ (decimalInt ~ ("." ~ digit.+).? ~ expPart.?
                                | ("." ~ digit.+) ~ expPart.?
                                )

  lazy val number: PackratParser[String] = consumed (hex | decimal)

  lazy val escapeSeq: PackratParser[Any] =
    "\\" ~ ( "u" ~ hexDigit ~ hexDigit ~ hexDigit ~ hexDigit
           | "x" ~ hexDigit ~ hexDigit
           | any
           )

  /**
   * Strings
   */
  lazy val string: PackratParser[String] = consumed ("""['"]""".r into { s => (escapeSeq | not(s) ~ any).* ~ s })

  /**
   * 15.10 Regular expressions
   */
  lazy val regexp: PackratParser[String]=
    consumed ("/" ~ reBody ~ "/" ~ "[a-zA-Z]*".r)

  lazy val reBody: PackratParser[Any] = reFirst ~ reChar.*
  lazy val reChar: PackratParser[Any] = reFirst | "*"

  lazy val reFirst: PackratParser[Any] =
    ( not ("*" | "/" | "[") ~ reClassChar
    | reClass
    | "]"
    )
  lazy val reClass: PackratParser[Any] = "[" ~ reClassChar.* ~ "]"


  lazy val reClassChar: PackratParser[Any] =
    escapeSeq | not ("]" | """\""" | "\n" | "\r") ~ any

}


/**
 * `parser1 ␣ parser2` combines Parsers while consuming any whitespaces between the two
 */
trait Parser extends Lexer {

  // Program
  // -------
  // annotating the topLevel parser with type PackratParser[ASTNode]
  // prevents nice error messages ("[1.1] failure: Base Failure")
  def _topLevel: Parser[Tree] =
    strippedPhrase (multiple (declaration)) ^^ Program

  def _declaration: PackratParser[Tree] =
    ( funcDecl | statement ) <~ (sc.?)

  // Statements
  // ----------
  def _block: PackratParser[Tree] =
    "{" ␣> multiple (declaration) <␣ "}" ^^ BlockStmt

  def _statement: PackratParser[Tree] =
    ( funcDecl

    | bindings // <~ sc

    | 'if ␣> ("(" ␣> expression <␣ ")") ␣ statement ␣ ('else ␣> statement).? ^^ IfStmt

    | 'while ␣> ("(" ␣> expression <␣ ")") ␣ statement ^^ WhileStmt

    | 'do ␣> (statement <␣ 'while) ␣ ("(" ␣> expression <␣ ")") ^^ DoWhileStmt

    | 'for ␣> ("(" ␣>
                 ( bindings | expression ).? ␣
                 (";" ␣> expression.?) ␣
                 (";" ␣> expression.?) <␣
             ")") ␣ statement ^^ ForStmt

    | 'for ␣> ("(" ␣>
                 ( bindings | leftExpr ) ␣
                 ('in ␣> expression) <␣
               ")") ␣ statement ^^ ForInStmt

    | 'switch ␣> ("(" ␣> expression <␣ ")") ␣ ("{" ␣>
                   multiple (
                     ( ('case ␣> expression <␣ ":") ␣ multiple (declaration) ^^ MatchingCase
                     | ('default ␣ ":") ␣> multiple (declaration) ^^ DefaultCase
                     )
                   ) <␣
      "}") ^^ SwitchStmt

    | 'break ~> (spacesNoNl ~> idLiteral.? <~ sc) ^^ BreakStmt

    | 'continue ~> (spacesNoNl ~> idLiteral.? <~ sc) ^^ ContinueStmt

    | 'throw ~> (spacesNoNl ~> expression <~ sc) ^^ ThrowStmt


    // catch is optional, if finally is provided
    | 'try ␣> block ␣ ('catch ␣> "(" ␣> idLiteral <␣ ")") ␣ block ␣
                      ('finally ␣> block).? ^^ {
        case body ~ cn ~ cb ~ Some(fb) => 
          TryStmt(body, Some(CatchBlock(cn, cb)), Some(FinallyBlock(fb)))
        case body ~ cn ~ cb ~ None => 
          TryStmt(body, Some(CatchBlock(cn, cb)), None)
      }
    | 'try ␣> block ␣ ('finally ␣> block) ^^ {
        case body ~ fb => TryStmt(body, None, Some(FinallyBlock(fb)))
      }

    | 'return ~> (spacesNoNl ~> expression.? <~ sc) ^^ ReturnStmt

    | 'with ␣> ("(" ␣> expression <␣ ")") ␣ statement ^^ WithStmt

    | idLiteral ␣ (":" ␣> statement) ^^ LabeledStmt

    | ";" ^^^ EmptyStmt

    // 12.4 Comma Operator
    // This allows the use of the comma-operator
    // (Or to be more precise, `expr` does this)
    //
    //     result += source, source = '';
    //
    // I had big trouble to prevent an endless
    // loop at this point. The solution was
    // to firstly consume all whitespaces and
    // then match `not sc`

    | spaces ~> (not ("{" | sc) ~> expression) <~ sc

    | block
    )

  // Expressions
  // -----------
  def _expression: PackratParser[Tree] =
  ( assignExpr ␣ ("," ␣> assignExpr ).+ ^^ {
      case first ~ rest => SequenceExpr(first :: rest)
    }
  | assignExpr
  )

  def _assignExpr: PackratParser[Tree] =
    ( leftExpr ␣ ( ">>>=" | ">>=" | "+="  | "-=" | "*="  | "/=" | "%="   | "<<="
                 | "^=" | "&&=" | "&=" | "||=" | "|=" | "="
                 ) ␣ assignExpr ^^ BinaryOpExpr
    | condExpr
    )

  // ternary operators
  def _condExpr: PackratParser[Tree] =
    ( orExpr ␣ ("?" ␣> assignExpr <␣ ":") ␣ assignExpr ^^ TernaryExpr
    | orExpr
    )

  // binary operators
  def _orExpr: PackratParser[Tree] =
    ( orExpr ␣ "||" ␣ andExpr ^^ BinaryOpExpr
    | andExpr
    )

  def _andExpr: PackratParser[Tree] =
    ( andExpr ␣ "&&" ␣ bitOrExpr ^^ BinaryOpExpr
    | bitOrExpr
    )

  def _bitOrExpr: PackratParser[Tree] =
    ( bitOrExpr ␣ "|" ␣ bitXorExpr ^^ BinaryOpExpr
    | bitXorExpr
    )

  def _bitXorExpr: PackratParser[Tree] =
    ( bitXorExpr ␣ "^" ␣ bitAndExpr ^^ BinaryOpExpr
    | bitAndExpr
    )

  def _bitAndExpr: PackratParser[Tree] =
    ( bitAndExpr ␣ "&" ␣ eqExpr ^^ BinaryOpExpr
    | eqExpr
    )

  def _eqExpr: PackratParser[Tree] =
    ( eqExpr ␣ ("===" | "==" | "!==" | "!=") ␣ relExpr ^^ BinaryOpExpr
    | relExpr
    )

  def _relExpr: PackratParser[Tree] =
    ( relExpr ␣ (">=" | ">" | "<=" | "<" | 'instanceof | 'in) ␣ shiftExpr ^^ BinaryOpExpr
    | shiftExpr
    )

  def _shiftExpr: PackratParser[Tree] =
    ( shiftExpr ␣ (">>>" | ">>" | "<<") ␣ addExpr ^^ BinaryOpExpr
    | addExpr
    )

  def _addExpr: PackratParser[Tree] =
    ( addExpr ␣ ("+" | "-") ␣ mulExpr ^^ BinaryOpExpr
    | mulExpr
    )

  def _mulExpr: PackratParser[Tree] =
    ( mulExpr ␣ ("*" | "/" | "%") ␣ prefixExpr ^^ BinaryOpExpr
    | prefixExpr
    )

  // unary operators
  def _prefixExpr: PackratParser[Tree] =
    ( ("++" | "--" ) ~ (spacesNoNl ~> unaryExpr) ^^ PrefixExpr
    | unaryExpr
    )

  def _unaryExpr: PackratParser[Tree] =
    ( ("!" | "~" | "+" | "-" | 'void | 'delete | 'typeof) ␣ prefixExpr ^^ PrefixExpr
    | postfixExpr
    )

  def _postfixExpr: PackratParser[Tree] =
    ( leftExpr ~ (spacesNoNl ~> ("++" | "--")) ^^ PostfixExpr
    | leftExpr
    )

  // different combinations of member-expressions, call expressions and new-expressions
  def _leftExpr: PackratParser[Tree] =
    ( 'new ␣> leftExpr ^^ NewExpr
    | accessExpr
    )

  def _accessExpr: PackratParser[Tree] =
    ( accessExpr ␣ callExpr ^^ CallExpr
    | accessExpr ␣ memberExpr ^^ MemberExpr
    | accessExpr ␣ nameAccessExpr ^^ NameAccessExpr
    | funcExpr
    | primExpr
    )

  def _callExpr: PackratParser[List[Tree]] =
    "(" ␣> listOf(assignExpr, ",") <␣ ")"

  def _memberExpr: PackratParser[Tree] =
    "[" ␣> expression <␣ "]"

  def _nameAccessExpr: PackratParser[Tree] =
    "." ␣> idLiteral


  // 11.1 Primary Expressions
  def _primExpr: PackratParser[Tree] =
    ( literal
    | "(" ␣> expression <␣ ")"
    )

  // Literals
  // --------
  def _idLiteral: PackratParser[Tree] =
    name ^^ AtomLit

  def _accessLiteral: PackratParser[Tree] =
    ( (string | number) ^^ AtomLit
    | idLiteral
    )

  def _literal: PackratParser[Tree] =
    ( idLiteral
    | accessLiteral
    | regexp ^^ AtomLit
    | arrayLiteral
    | objectLiteral
    | 'this ^^^ AtomLit("this")
    )

  // 11.1.4 Array Literals
  def _arrayLiteral: PackratParser[Tree] =
    ( "[" ␣ "]" ^^^ ArrayExpr(Nil)
    | "[" ␣> listOf(arrayEl, ",") <␣ "]" ^^ ArrayExpr
    )

  def _arrayEl: PackratParser[Tree] =
    ( assignExpr
    | result (AtomLit("undefined"))
    )

  // 11.1.5 Object Literals
  // TODO include getters and setters here
  def _objectLiteral: PackratParser[Tree] =
    "{" ␣> listOf(objBinding, ",") <␣ "}" ^^ ObjectExpr

  def _objBinding: PackratParser[Tree] =
    (accessLiteral <␣ ":") ␣ assignExpr ^^ PropertyBinding

  // Functions
  def _funcDecl: PackratParser[Tree] =
    ('function ␣> idLiteral) ␣ ("(" ␣> funcArgs <␣ ")") ␣ block ^^ FunctionDecl

  def _funcExpr: PackratParser[Tree] =
    'function ␣> idLiteral.? ␣ ("(" ␣> funcArgs <␣ ")") ␣ block ^^ FunctionExpr


  def _funcArgs: PackratParser[List[Tree]] =
    listOf(idLiteral, ",")

  // Variable Declarations
  def _bindings: PackratParser[Tree] =
    'var ␣> listOf(binding, ",") ^^ VarDeclStmt

  def _binding: PackratParser[Tree] =
    idLiteral ␣ ( "=" ␣> assignExpr
                | result (AtomLit("undefined"))
                ) ^^ VarBinding


  lazy val topLevel       = _topLevel
  lazy val declaration    = _declaration
  lazy val block          = _block
  lazy val statement      = _statement
  lazy val expression     = _expression
  lazy val assignExpr     = _assignExpr
  lazy val condExpr       = _condExpr
  lazy val orExpr         = _orExpr
  lazy val andExpr        = _andExpr
  lazy val bitOrExpr      = _bitOrExpr
  lazy val bitXorExpr     = _bitXorExpr
  lazy val bitAndExpr     = _bitAndExpr
  lazy val eqExpr         = _eqExpr
  lazy val relExpr        = _relExpr
  lazy val shiftExpr      = _shiftExpr
  lazy val addExpr        = _addExpr
  lazy val mulExpr        = _mulExpr
  lazy val prefixExpr     = _prefixExpr
  lazy val unaryExpr      = _unaryExpr
  lazy val postfixExpr    = _postfixExpr
  lazy val leftExpr       = _leftExpr
  lazy val accessExpr     = _accessExpr
  lazy val callExpr       = _callExpr
  lazy val memberExpr     = _memberExpr
  lazy val nameAccessExpr = _nameAccessExpr
  lazy val primExpr       = _primExpr
  lazy val idLiteral      = _idLiteral
  lazy val accessLiteral  = _accessLiteral
  lazy val literal        = _literal
  lazy val arrayLiteral   = _arrayLiteral
  lazy val arrayEl        = _arrayEl
  lazy val objectLiteral  = _objectLiteral
  lazy val objBinding     = _objBinding
  lazy val funcDecl       = _funcDecl
  lazy val funcExpr       = _funcExpr
  lazy val funcArgs       = _funcArgs
  lazy val bindings       = _bindings
  lazy val binding        = _binding

}