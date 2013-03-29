package choicecalculus
package parser

import org.kiama.util.PositionedParserUtilities
import utility.{ ParserUtils, WhitespaceAwareParser }


/**
 * Test to write:
 * 
 * return typeof this === 'function'
 * var foo = "abcd"
 * this.bar
 * 
 * 
 * Next thing to do:
 * 1. Implement pretty printer
 * 2. implement regexp
 * 3. implement real identifiers
 * 
 */


// This Lexer is just approximate! Does not follow the spec, yet!
trait JavaScriptLexer extends PositionedParserUtilities with ParserUtils with WhitespaceAwareParser {
  
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
  
  lazy val comment: PackratParser[Any] = 
    singleline | multiline
  
  lazy val space: PackratParser[Any] =
    whitespace | linebreak | comment
  
  lazy val spacesNoNl: PackratParser[Any] = (not (linebreak) ~ space).*
 
  
  // "automatic semicolon insertion"
  lazy val sc: PackratParser[Any] = 
    (spacesNoNl ~ (linebreak | lookahead("}") | EOS)
    | ";"
    )
  
    
  /**
   * Identifiers
   */  
  lazy val nameFirst  = """[a-zA-Z$_]""".r
  lazy val nameRest   = """[a-zA-Z0-9$_]""".r
  lazy val identifier = "%s(%s)*".format(nameFirst, nameRest).r
  
  lazy val name: PackratParser[String] = 
    matches(identifier, "This is a protected keyword!") unless { keywords contains Symbol(_) }
  
  lazy val keyword: PackratParser[String] =
    matches(identifier, "Expected keyword") when { keywords contains Symbol(_) }
  
  
  /**
   * Numbers
   */
  lazy val digit    = """[0-9]""".r
  lazy val hexDigit = """[0-9a-fA-F]""".r
  lazy val hex      = "0x" ~ hexDigit.+
  
  lazy val decimalInt = """(0|[1-9][0-9]*)""".r
  lazy val expPart    = """[eE][+\-]?[0-9]+""".r
  
  lazy val decimal    = "-".? ~ (decimalInt ~ ("." ~ digit.+).? ~ expPart.?
                                | ("." ~ digit.+) ~ expPart.?
                                )
                                
  lazy val number: PackratParser[String] = consumed (hex | decimal) 
  
  
  lazy val string: PackratParser[String] = ( consumed ("""["][^"]*["]""".r)
                                           | consumed ("""['][^']*[']""".r)
                                           )  
}

/**
 * `parser1 ␣ parser2` combines Parsers while consuming any whitespaces between the two 
 */
trait JavaScriptParser extends PositionedParserUtilities with ParserUtils with JavaScriptLexer {

  import ast._
  
  // Literals
  lazy val stringLiteral: PackratParser[Literal] =
    string ^^ Literal
    
  lazy val numberLiteral: PackratParser[Literal] =
    number ^^ Literal
    
  lazy val idLiteral: PackratParser[Literal] =
    name ^^ Literal
    
  lazy val literal: PackratParser[Literal] =
    idLiteral | stringLiteral | numberLiteral
  
  // TODO don't forget comma operator here!
  lazy val expression: PackratParser[Expression] = assignExpr
  
  lazy val assignExpr: PackratParser[Expression] = 
    ( leftExpr ␣ ( ">>>=" | ">>=" | "+="  | "-=" | "*="  | "/=" | "%="   | "<<=" 
                 | "^=" | "&&=" | "&=" | "||=" | "|=" | "=" 
                 ) ␣ assignExpr ^^ BinaryOpExpr
    | condExpr
    )
  
  // ternary operators
  lazy val condExpr: PackratParser[Expression] = 
    ( orExpr ␣ ( "?" ␣> assignExpr <␣ ":") ␣ assignExpr ^^ TernaryExpr
    | orExpr
    )
                      
  // binary operators
  lazy val orExpr: PackratParser[Expression] = 
    ( orExpr ␣ "||" ␣ andExpr ^^ BinaryOpExpr
    | andExpr
    )
                        
  lazy val andExpr: PackratParser[Expression] = 
    ( andExpr ␣ "&&" ␣ bitOrExpr ^^ BinaryOpExpr
    | bitOrExpr
    )

  lazy val bitOrExpr: PackratParser[Expression] = 
    ( bitOrExpr ␣ "|" ␣ bitXorExpr ^^ BinaryOpExpr
    | bitXorExpr
    )

  lazy val bitXorExpr: PackratParser[Expression] = 
    ( bitXorExpr ␣ "^" ␣ bitAndExpr ^^ BinaryOpExpr
    | bitAndExpr
    )
    
  lazy val bitAndExpr: PackratParser[Expression] = 
    ( bitAndExpr ␣ "&" ␣ eqExpr ^^ BinaryOpExpr
    | eqExpr
    )
  
  lazy val eqExpr: PackratParser[Expression] = 
    ( eqExpr ␣ ( "===" | "==" | "!==" | "!=") ␣ relExpr ^^ BinaryOpExpr
    | relExpr
    )
    
  lazy val relExpr: PackratParser[Expression] = 
    ( relExpr ␣ ( ">=" | ">" | "<=" | "<" | 'instanceof | 'in) ␣ shiftExpr ^^ BinaryOpExpr
    | shiftExpr
    )
    
  lazy val shiftExpr: PackratParser[Expression] = 
    ( shiftExpr ␣ ( ">>>" | ">>" | "<<" ) ␣ addExpr ^^ BinaryOpExpr
    | addExpr
    )
    
  lazy val addExpr: PackratParser[Expression] =
    ( addExpr ␣ ( "+" | "-" ) ␣ mulExpr ^^ BinaryOpExpr
    | mulExpr
    )
    
  lazy val mulExpr: PackratParser[Expression] =
    ( mulExpr ␣ ( "*" | "/" | "%" ) ␣ prefixExpr ^^ BinaryOpExpr
    | prefixExpr
    )
    
    
  // unary operators
  lazy val prefixExpr: PackratParser[Expression] =
    ( ("++" | "--" ) ~ (spacesNoNl ~> unaryExpr) ^^ PrefixExpr
    | unaryExpr
    )
    
  lazy val unaryExpr: PackratParser[Expression] =
    ( ( "!" | "~" | "+" | "-" | 'void | 'delete | 'typeof) ␣ prefixExpr ^^ PrefixExpr
    | postfixExpr
    )
  
  lazy val postfixExpr: PackratParser[Expression] =
    ( leftExpr ~ (spacesNoNl ~> ("++" | "--")) ^^ PostfixExpr
    | leftExpr
    )       
  
  // different combinations of member-expressions, call expressions and new-expressions
  lazy val leftExpr: PackratParser[Expression] =
    ( 'new ␣> leftExpr ^^ NewExpr
    | accessExpr
    )
    
  lazy val accessExpr: PackratParser[Expression] =
    ( accessExpr ␣ callExpr ^^ CallExpr
    | accessExpr ␣ memberExpr ^^ MemberExpr
    | accessExpr ␣ nameAccessExpr ^^ NameAccessExpr
    | funcExpr
    | primExpr
    )
    
  
  lazy val callExpr: PackratParser[List[Expression]] =
    "(" ␣> listOf(assignExpr, ",") <␣ ")"
    
  
  lazy val memberExpr: PackratParser[Expression] =
    "[" ␣> expression <␣ "]"
  
  lazy val nameAccessExpr: PackratParser[Literal] =
    "." ␣> idLiteral


  // 11.1 Primary Expressions
  lazy val primExpr: PackratParser[Expression] =
    ( objectLiteral
    | arrayLiteral
    | "(" ␣> expression <␣ ")" ^^ GroupExpr    
    | 'this ^^^ Literal("this")
    | literal
    )
  
  // 11.1.4 Array Literals
  // TODO run tests, to check whether this encoding of elision works
  lazy val arrayLiteral: PackratParser[Expression] =
    "[" ␣> listOf(arrayEl, ",") <␣ "]" ^^ ArrayExpr
  
  lazy val arrayEl: PackratParser[Expression] =
    ( assignExpr | result (Literal("undefined")) )
    
  
  
  
  // 11.1.5 Object Literals
  // TODO include getters and setters here
  lazy val objectLiteral: PackratParser[Expression] = 
    "{" ␣> listOf(objBinding, ",") <␣ "}" ^^ ObjectExpr
  
  lazy val objBinding: PackratParser[PropertyBinding] = 
    (literal <␣ ":") ␣ assignExpr ^^ PropertyBinding
  
  // Functions
  lazy val funcDecl: PackratParser[Expression] = 
    ('function ␣> idLiteral) ␣ ("(" ␣> funcArgs <␣ ")") ␣ block ^^ FunctionDecl
    
  lazy val funcExpr: PackratParser[Expression] =
    ( 'function ␣> ("(" ␣> funcArgs <␣ ")") ␣ block ^^ FunctionExpr
    | funcDecl
    )
  
  lazy val funcArgs: PackratParser[List[Literal]] =
    listOf(idLiteral, ",")

    
  // Variable Declarations
  lazy val bindings: PackratParser[Statement] =
    'var ␣> listOf(binding, ",") ^^ VarDeclStmt
    
  lazy val binding: PackratParser[VarBinding] = 
    idLiteral ␣ ( "=" ␣> assignExpr
                | result (Literal("undefined"))
                ) ^^ VarBinding
   

  // Block Statement
  lazy val block: PackratParser[BlockStmt] = 
    "{" ␣> multiple (srcElem) <␣ "}" ^^ BlockStmt


  // 12. Statements
  lazy val statement: PackratParser[Statement] = 
    ( bindings <~ sc
        
    | 'if ␣> ("(" ␣> expression <␣ ")") ␣ statement ␣ ('else ␣> statement).? ^^ IfStmt
    
    | 'while ␣> ("(" ␣> expression <␣ ")") ␣ statement ^^ WhileStmt
    
    | 'do ␣> (statement <␣ 'while) ␣ ("(" ␣> expression <␣ ")") <␣ sc ^^ DoWhileStmt
    
    | 'for ␣> ("(" ␣>
                 ( bindings | expression ) ␣ 
                 (";" ␣> expression.?) ␣ 
                 (";" ␣> expression.?) <␣
             ")") ␣ statement ^^ ForStmt
             
    | 'for ␣> ("(" ␣> 
                 ( bindings | leftExpr ) ␣
                 ('in ␣> expression) <␣
               ")") ␣ statement ^^ ForInStmt
               
    | 'switch ␣> ("(" ␣> expression <␣ ")") ␣ ("{" ␣> 
                   multiple (
                     ( ('case ␣> expression <␣ ":") ␣ multiple (srcElem) ^^ MatchingCase
                     | ('default ␣ ":") ␣> multiple (srcElem) ^^ DefaultCase
                     )
                   ) <␣
      "}") ^^ SwitchStmt
                     
    | 'break ~> (spacesNoNl ~> idLiteral.? <~ sc) ^^ BreakStmt
    
    | 'continue ~> (spacesNoNl ~> idLiteral.? <~ sc) ^^ ContinueStmt
                   
    | 'throw ~> (spacesNoNl ~> expression <~ sc) ^^ ThrowStmt
    
    
    // catch is optional, if finally is provided
    | 'try ␣> block ␣ ( 'catch ␣> "(" ␣> idLiteral <␣ ")") ␣ block ␣
                      ( 'finally ␣> block ).? ^^ {
        case body ~ cn ~ cb ~ Some(fb) => TryStmt(body, Some(CatchBlock(cn, cb)), Some(FinallyBlock(fb)))
        case body ~ cn ~ cb ~ None => TryStmt(body, Some(CatchBlock(cn, cb)), None)
      }
    | 'try ␣> block ␣ ("finally" ␣> block) ^^ {
        case body ~ fb => TryStmt(body, None, Some(FinallyBlock(fb)))
      }
      
    // TODO Check whether the whitespace handling here is correct!
    | 'return ␣> expression <~ sc ^^ ReturnStmt
    
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
    
    | spaces ~> ( not ("{" | 'function | sc) ~> expression) <~ sc
    
    | block
    )
  
  // Source Elements
  lazy val srcElem: PackratParser[Statement] = funcDecl | statement

  // Program
  //
  lazy val topLevel: PackratParser[ASTNode] = multiple (srcElem) <~ spaces <~ EOS ^^ Program
  
}