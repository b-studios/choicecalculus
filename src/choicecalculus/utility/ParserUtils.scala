package choicecalculus
package utility

import scala.util.parsing.combinator.Parsers
import org.kiama.util.PositionedParserUtilities
import scala.util.parsing.combinator.RegexParsers
import scala.util.matching.Regex

trait ParserUtils { self: Parsers =>
  
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
  
   // my own helper function to work with predicates
  protected sealed case class EnsureControl[+T](base: Parser[T], msg: String) {
    def when(pred: T => Boolean): Parser[T] = base >> (
      (n) => if (pred(n)) success(n) else failure(msg)
    )
    def unless(pred: T => Boolean): Parser[T] = when((n) => !pred(n))
    def always: Parser[T] = base
  }
  def matches[T](parser: Parser[T], msg: String = "Did not match predicate"): EnsureControl[T] = EnsureControl(parser, msg)
  
  // can be used it like: 'do ~ "{" ~ ... ~ "}" ~ 'while
  def keyword: Parser[String]
  implicit def sym2keyword(s: Symbol): Parser[String] = matches(keyword) when { Symbol(_) == s }
  
  def combine(regs: Regex*) =
    ("(%s" * regs.size).format(regs:_*).r
  
  def lookahead[T](parser: Parser[T]) =
    not(not(parser))
  
  // http://scalaholla.wordpress.com/tag/end-of-input/
  def EOS: Parser[Any] = new Parser[Any] {
    def apply(in: Input) = {
      if (in.atEnd) new Success( "EOS", in )
      else Failure("Expected end of stream", in)
    }
  }
  
}

trait WhitespaceAwareParser { self: PositionedParserUtilities with ParserUtils =>
  
  override val skipWhitespace = false
  
  val space: Parser[Any]
  
  lazy val spaces = rep(space)
  
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
  implicit def symbol2WSParser(s: Symbol) = WSParserWrapper(matches(keyword) when { Symbol(_) == s })
}