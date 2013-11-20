package choicecalculus
package recovery

private[recovery] trait Choices[T] {

  abstract class CCExpr

  case class CCChoice(dim: Symbol, cases: List[CCCase]) extends CCExpr {
    override def toString: String = dim.name + "<" + cases.mkString(", ") + ">"
  }
  case class CCCase(tag: Symbol, value: CCExpr) {
    override def toString: String = tag.name + ":" + value.toString
  }  
  case class Literal(value: T) extends CCExpr

}
