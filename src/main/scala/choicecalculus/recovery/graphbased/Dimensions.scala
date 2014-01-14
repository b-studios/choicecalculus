package choicecalculus
package recovery
package graphbased

private[recovery] trait Dimensions {

  var dimensions: Map[Symbol, List[Symbol]] = Map.empty
  var _dimcount = 1

  def createDimension(arity: Int): (Symbol, List[Symbol]) = {
    val dim = (Symbol("DIM_" + _dimcount), (1 to arity).map { i => Symbol("TAG_" + i) }.toList)
    dimensions = dimensions + dim
    _dimcount += 1
    dim
  }

}
