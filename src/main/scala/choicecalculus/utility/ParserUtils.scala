package choicecalculus
package utility

import scala.util.parsing.combinator.Parsers
import org.kiama.util.PositionedParserUtilities
import scala.util.matching.Regex

trait ParserUtils extends PositionedParserUtilities {
  
  override val skipWhitespace = false
  
  val space: Parser[Any]
  
  lazy val spaces = rep(space)
  
  def strippedPhrase[T](body: Parser[T]): Parser[T] = 
    phrase(spaces ~> body <~ spaces)
  
  case class WSParserWrapper[T](parser: Parser[T]) {
    def ␣[U](other: => Parser[U]) = 
      parser ~ (spaces ~> other)
      
    def ␣>[U](other: => Parser[U]) = 
      parser ~> (spaces ~> other)
      
    def <␣[U](other: => Parser[U]) = 
      parser <~ (spaces ~ other)
  }
  
  // could also be called repsepWithSpaces
  def listOf[T](p: => Parser[T], q: => Parser[Any]): Parser[List[T]] =
    repsep(p, spaces ~ q ~ spaces)
  
  // could also be called repWithSpaces
  def multiple[T](p: => Parser[T]): Parser[List[T]] = rep1(spaces ~> p) | success(List())
  
  implicit def parser2WSParser[U](parser: Parser[U]) = WSParserWrapper(parser)
  implicit def string2WSParser(s: String) = WSParserWrapper(literal(s))
  implicit def symbol2WSParser(s: Symbol) = WSParserWrapper(keyword(s))
  
  
  // only works for strings.
  // http://stackoverflow.com/questions/9020852/scala-combinator-parser-to-keep-original-input
  def consumed [U](p: => Parser[U]): Parser[String] = new Parser[String] {
    def apply(in: Input) = p(in) match {
      case Success(result, next) =>
        val parsedString = in.source.subSequence(in.offset, next.offset).toString
        Success(parsedString, next)
      case other: NoSuccess => other
    }
  }
 
  // can be used it like: 'do ~ "{" ~ ... ~ "}" ~ 'while
  def keyword(s: Symbol): Parser[String]
  implicit def sym2keyword(s: Symbol): Parser[String] = keyword(s)
  
  def combine(regs: Regex*) =
    ("(%s" * regs.size).format(regs:_*).r
  
}