package choicecalculus
package utility

/**
 * Attribution preserving rewriter
 */
trait AttributableRewriter extends org.kiama.rewriting.CallbackRewriter {

  import org.kiama.attribution.Attributable
  
  def rewriting[T] (oldTerm : T, newTerm : T) : T = { 
    (oldTerm, newTerm) match {
      case (o: Attributable, n: Attributable) => n.initTreeProperties
      case _ =>
    }
    newTerm
  }
}

object AttributableRewriter extends AttributableRewriter {
  
  case class UtilStrategy(base: Strategy) {
    def then(other: => Strategy): Strategy = attempt(base) <* other
  }
  
  implicit def strategy2utilStrategy(s: Strategy): UtilStrategy = UtilStrategy(s)
  
}