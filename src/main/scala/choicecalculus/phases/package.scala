package choicecalculus
package phases

object Phase extends Enumeration {
  val Parser, Reader, Namer, DimensionChecker, Evaluator, Generator = Value
}