package choicecalculus
package utility

case class Node(name: Symbol) {

  def ---(other: Node): Edge = Edge(this, other)
  
}

// undirected edge
// TODO implement hashCode
case class Edge(left: Node, right: Node) {

def connects(node1: Node, node2: Node): Boolean =
  (left == node1 && right == node2) || (left == node2 && right == node1)

  override def equals(that: Any): Boolean = that match {
    case that: Edge if that.canEqual(this) => that.connects(left, right)
    case _ => false
  }
}