package choicecalculus
package semantics

import scala.collection.mutable

trait ChoiceGraph {

  abstract class GraphNode
  case class ChoiceNode(dim: Symbol, tag: Symbol) extends GraphNode {
    override def toString = "%s.%s".format(dim.name, tag.name)
  }
  // represents ø
  case object NoChoice extends GraphNode {
    override def toString = "ø"
  }
  
  /**
   * The simplest version of the choice graph just stores the edges for each node
   * This makes merging of graphs trivial.
   * 
   *                        +--- C.e ---+
   *                       /             \
   *      A.a ---> B.c ---+               +---> ø
   *                       \             /
   *                        +--- C.f ---+
   *
   */
  type Graph = Map[GraphNode, Set[GraphNode]]
  
  case class ChoiceGraph(
      startNodes: Set[GraphNode],
      edges: Graph) {

  
    def isEmpty = edges.isEmpty
    
    def ++(other: ChoiceGraph): ChoiceGraph = {
      
      var mergedEdges = edges
      
      for ( (node, targets) <- other.edges ) {
        if (hasCycle(node, targets, mergedEdges))
          sys error "Error while merging choice graphs -> resulting graph will contain cycles!"
        
        if (edges contains node)
          mergedEdges += (node -> (edges(node) ++ targets))
        else
          mergedEdges += (node -> targets)        
      } 
      
      ChoiceGraph(this.startNodes ++ other.startNodes, mergedEdges)
    }

    def from(dim: Symbol, tag: Symbol): ChoiceGraph = {
      
      val newStartNode = ChoiceNode(dim, tag)
      
      if (hasCycle(newStartNode, startNodes, edges))
        sys error "Graph contains cycles"
      
      val existingEdges = if (edges contains newStartNode) {
        edges(newStartNode)
      } else {
        Set() 
      }
      
      ChoiceGraph(
          Set(newStartNode), 
          edges + (newStartNode -> (existingEdges ++ startNodes))
      )
    }
    
    def select(dim: Symbol, tag: Symbol): ChoiceGraph = {
      val toSelect = ChoiceNode(dim, tag)
      
      if (! (startNodes contains toSelect) )
        sys error "cannot selected (maybe nested) choice %s in graph %s".format(toSelect, this)
      
      ChoiceGraph((startNodes - toSelect) ++ edges(toSelect), edges)
    }
    
    
    override def toString = 
      "Start-nodes: %s\nEdges:\n%s\n".format(
          startNodes mkString ", ", 
          edges.map((mapping) => "  %s → %s".format(mapping._1, mapping._2 mkString ", ")) mkString "\n" )

    
    private def hasCycle(
        target: GraphNode, 
        startingFrom: Set[GraphNode], 
        edges: Graph, 
        visited: Set[GraphNode] = Set()): Boolean = {
      
      if (visited contains target)
        return true
      
      if (startingFrom.isEmpty)
        return false
     
      val targets = (for {
        node <- startingFrom
        if edges contains node
      } yield edges(node)).flatten
        
      hasCycle(target, targets, edges, visited ++ startingFrom)
    }   
    
  }
  object ChoiceGraph {
    def apply(): ChoiceGraph = ChoiceGraph(Set(NoChoice), Map())
    def empty = ChoiceGraph(Set(NoChoice), Map())
  }
  
}