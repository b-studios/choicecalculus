package choicecalculus
package recovery
package graphbased

import scala.collection.mutable
import utility.{ Node, Edge, Table }

private[recovery] case class ProblemGraph(variables: Map[Symbol, Set[Node]], edges: Set[Edge]) {
  lazy val nodes = edges.flatMap { case Edge(v1, v2) => Set(v1, v2) }
}

private[recovery] trait ProblemGraphBuilder[T] {

  type Variable = Symbol

  val names: mutable.Map[Variable, mutable.Map[T, Node]] = mutable.Map.empty
  private var index = 0;

  def newName(v: Variable): Symbol = {
    index = index + 1;
    Symbol(v.name + index)
  }

  // Allows the bidirectional mapping of table entries to graph nodes
  object NamedInstance {
    def apply(variable: Variable, value: T): Node =
      names.getOrElseUpdate(variable, mutable.Map.empty)
        .getOrElseUpdate(value, Node(newName(variable)))

    def apply(instance: (Variable, T)): Node = apply(instance._1, instance._2)

    // BROKEN
    def unapply(n: Node): Option[(Variable, T)] =
      (for {
        (variable, values) <- names
        (value, node) <- values
        if node == n
      } yield (variable, value)).headOption
  }

  def createFromTable(table: Table[Variable, T]): ProblemGraph = {

    var edges: Set[Edge] = Set.empty

    for {
      row <- table.rowsWithHeaders
      instance1 <- row
      instance2 <- row
      if instance1 != instance2
    } edges = edges + Edge(NamedInstance(instance1), NamedInstance(instance2))

    // now extract all nodes, that appear in edges
    val variables = edges.flatMap {
      case Edge(v1, v2) => Set(v1, v2)
    }.groupBy {
      case NamedInstance(v, _) => v
    }

    ProblemGraph(variables, edges)
  }
}