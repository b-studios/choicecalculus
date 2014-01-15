package choicecalculus
package utility

import org.kiama.output.ParenPrettyPrinter

object DebugPrettyPrinter extends ParenPrettyPrinter with org.kiama.output.PrettyPrinter {

  override val defaultWidth = 80

  def token(name: String, klass: String): Doc =
    text(<span class="{klass}">{ name }</span>.toString)

  def toDoc(e: Any): Doc = e match {
    case s: Seq[_] => lbracket <> fillsep(s.map(toDoc), comma) <> rbracket
    case p: Product => token(p.productPrefix, "product") <>
      lparen <\> nest(softline <> fillsep(p.productIterator.toList.map(toDoc), comma)) <> rparen
    case _ => e.toString
  }

}

object XMLPrettyPrinter {

  def list(l: List[Any], sep: scala.xml.Node): List[scala.xml.Node] = l match {
    case Nil => Nil
    case f :: Nil => pretty(f) :: Nil
    case f :: rest => pretty(f) :: sep :: list(rest, sep)
  }

  def token(name: String, content: String) =
    <span class={ "token " + name }>{ content }</span>

  def punct(content: String) =
    token("punct", content)

  val lparen = punct("(")
  val rparen = punct(")")
  val lbracket = punct("[")
  val rbracket = punct("]")
  val comma = punct(", ")

  def pretty(e: Any): scala.xml.Node = e match {
    case s: Seq[_] =>
      <span class="list ref" data-id={ System.identityHashCode(s).toString }>{ lbracket }{ list(s.toList, comma) }{ rbracket }</span>

    case t: Product if t.productPrefix.startsWith("Tuple") =>
      <span class="tuple ref" data-id={ System.identityHashCode(t).toString }>{ lparen }{ list(t.productIterator.toList, comma) }{ rparen }</span>

    case p: Product =>
      <span class={ "product size%s ref".format(p.productArity) } data-id={ System.identityHashCode(p).toString }>{ token("name", p.productPrefix) }{ lparen }<span class="contents">{ list(p.productIterator.toList, comma) }</span>{ rparen }</span>

    case sym: Symbol =>
      token("symbol", sym.name)

    case s: String =>
      token("string", s)

    case _ =>
      <span class={ "%s ref".format(e.getClass().toString) } data-id={ System.identityHashCode(e).toString }>{ e.toString }</span>
  }

}

// [Uncached]AttributionCore
object Attribution extends org.kiama.attribution.AttributionCore {}

/**
 * Attribution preserving rewriter
 */
trait AttributableRewriter extends org.kiama.rewriting.CallbackRewriter {

  import org.kiama.attribution.Attributable
  import scala.xml._
  import org.kiama.rewriting.Strategy
  import lang.trees.Tree

  def rewriting[T](oldTerm: T, newTerm: T): T = {
    (oldTerm, newTerm) match {
      case (o: Attributable, n: Attributable) => n.initTreeProperties
      case _ =>
    }
    newTerm
  }

  case class AttributionFix[T <: Attributable](a: T) {
    def fixAttr(): T = {
      a.initTreeProperties
      a
    }
  }
  implicit def attr2attrFix[T <: Attributable](a: T): AttributionFix[T] = AttributionFix(a)

  case class UtilStrategy(base: Strategy) {
    def then(other: => Strategy): Strategy =
      attempt(base) <* other

    def andFinally(other: => Strategy): Strategy =
      (base <* attempt(other) + (other <* fail))
  }

  implicit def strategy2utilStrategy(s: Strategy): UtilStrategy = UtilStrategy(s)

}
object AttributableRewriter extends AttributableRewriter {}

trait DebugRewriter extends AttributableRewriter {

  import org.bitbucket.inkytonik.dsprofile.Events.{ Event, Start }
  import org.bitbucket.inkytonik.dsprofile.Events
  import org.kiama.rewriting.Strategy

  Events.profiling = true

  val debugFile = "debug.html"

  val outputBuffer = new scala.collection.mutable.ListBuffer[String]();

  def attributeInfo(term: Any): Option[String] = None

  def debugging(before: Any)(block: => Any) {
    var beforeString = XMLPrettyPrinter.pretty(before);
    outputBuffer.clear
    outputBuffer.append("""
<!DOCTYPE>
<html>
<head>
<link rel="stylesheet" href="debug.css"/>
</head>
<body>
<div class="start"><code>   
    %s
</code></div>
<div id="rules">
    """.format(beforeString))

    val result = block

    outputBuffer.append("""
</div>
        
<div class="end">%s</div>
<script src="http://ajax.googleapis.com/ajax/libs/jquery/1.10.1/jquery.min.js"></script>
<script src="debug.js"></script>
</body>
</html>""".format(XMLPrettyPrinter.pretty(result)))

    val f = new java.io.FileWriter(debugFile);
    f.write(outputBuffer mkString "\n")
    f.close
  }

  def writeToDebug(s: scala.xml.Elem) {
    outputBuffer.append(s.toString)
  }

  def writeToDebug(s: String) {
    outputBuffer.append(s)
  }

  def named(name: String, s: Strategy): Strategy =
    new Strategy(name) {
      val body = (t: Any) => s(t)
    }

  def analyze(s: Strategy)(f: PartialFunction[(Strategy, Any, Any), Unit]): Strategy =
    new Strategy("report") {
      val body = (before: Any) => {
        val after = s(before)
        if (f.isDefinedAt((s, before, after))) {
          f(s, before, after)
        }
        after
      }
    }

  // add a switch for verbose (not checking whether oldTerm == newTerm)
  override def rewriting[T](oldTerm: T, newTerm: T): T = {
    if (oldTerm == newTerm) {
      return newTerm
    }
    val lastStartEvent = Events.events.filter { (e: Event) =>
      e.kind == Start && e.dimensions.get("subject") == Some(oldTerm)
    }.last

    outputBuffer.append(<div class="rule success">
                          <span class="name">{ lastStartEvent.dimensions.get("strategy").getOrElse("unknown") }</span>
                          <code class="before">{ XMLPrettyPrinter.pretty(oldTerm) }</code>
                          <code class="after">{ XMLPrettyPrinter.pretty(newTerm) }</code>
                        </div>.toString)
    super.rewriting(oldTerm, newTerm)
  }
}
object DebugRewriter extends DebugRewriter {}