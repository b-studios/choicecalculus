package choicecalculus
package semantics

import scala.collection.mutable

/**
 * Notes: Representing edges as own class makes rewriting extremely tedious!
 */
trait DimensionGraph {

  import org.kiama.util.Messaging.message
  import choicecalculus.ast.ASTNode
  
  abstract class GraphNode {
    
    import scala.collection.GenTraversableOnce
    
    def edges: Map[Symbol, Set[GraphNode]]
    
    lazy val subnodes = edges.flatMap(_._2)
    
    // needed to be used in for-comprehensions
    def flatMap[B](f: GraphNode => GenTraversableOnce[B]): List[B] = subnodes.foldLeft[List[B]](Nil) {
      (old, node) => f.apply(node).toList ++ old ++ node.flatMap[B](f)
    }
    
    def map[B](f: GraphNode => B): List[B] = subnodes.foldLeft[List[B]](Nil) { 
      (old, node) => f.apply(node) :: (old ++ node.map[B](f)) 
    }
    
    def foreach(f: GraphNode => Unit): Unit = subnodes.foreach {
      (node) => f.apply(node); node.foreach(f)
    }
    
  }
  
  // Used to represent dependent, but unbound dimensions
  case class ChoiceNode(dim: Symbol, edges: Map[Symbol, Set[GraphNode]]) extends GraphNode {
    override def toString = edges.map( (edge) => "(%s) --%s--> %s".format(dim.name, edge._1, edge._2) ) mkString "\n"
  }
  
  case class DimensionNode(name: Symbol, tags: List[Symbol], edges: Map[Symbol, Set[GraphNode]]) extends GraphNode {
    override def toString = edges.map( (edge) => "%s<%s> --%s--> %s".format(name.name, tags mkString ", ", edge._1, edge._2)) mkString "\n"
  }

  
  /**
   * Desired usage:
   * (DimensionGraph.empty.fromChoice('C, 'c).declareDimension('C, List('c))
   *                  .fromChoice('B, 'b).fromChoice('A, 'a) ++
   * 
   * DimensionGraph.empty.fromChoice('D, 'd).declareDimension('D, List('d))
   *                  .fromChoice('A, 'a).fromChoice('B, 'b))
   * 
   * 		.declareDimension('A, List('a))
   * 		.declareDimension('B, List('b))
   */
  
  case class DimensionGraph(nodes: Set[GraphNode]) {
    
    
    def isEmpty = nodes.isEmpty
    
    def getDimensionNodes(name: Symbol): List[DimensionNode] = nodes.collect {
      case node@DimensionNode(n,_,_) if n == name => node
    }.toList
    
    def getChoiceNodes(dim: Symbol): List[ChoiceNode] = nodes.collect {
      case node@ChoiceNode(d,_) if d == dim => node
    }.toList
    
    def canSelect(dim: Symbol) = {
      //printf("Calling 'canSelect(%s)'=%s on graph: %s node: %s\n", dim,getDimensionNodes(dim).size == 1, this, e)
      this.getDimensionNodes(dim).size == 1
    }    
    
    def fullyConfigured = 
      nodes == Set()    
    
    /**
     * (A) --a--> B<> --b--> C<>
     *    \--b--> D<>
     * 
     * becomes
     * 
     * A<> --a--> B<> --b--> C<>
     *    \--b--> D<>
     */
    def declareDimension(name: Symbol, tags: List[Symbol])(ast: ASTNode): DimensionGraph = getChoiceNodes(name) match {
        
      // nothing needs to be done
      case Nil => {
        message(ast, "INFO: Your declared dimension %s is never used. Maybe remove it?".format(name.name))
        return this
      }
      
      case choices if choices.size > 1 => {
        message(ast, "ERROR: Nested choices of the same dimension are not supported! (%s)".format(name.name))
        return this       
      }
      
      // use `new` here to create different dimension nodes every time
      case choice :: Nil => {
        
        choice.edges.collect {
          case (tag, _) if !(tags contains tag) => 
            message(ast, "ERROR: The used tag '%s' is not declared in this dimension".format(tag.name))
        }
        
        DimensionGraph(nodes - choice + new DimensionNode(name, tags, choice.edges))
      }
    }
    
    
    def fromChoice(dim: Symbol, tag: Symbol)(ast: ASTNode): DimensionGraph = getChoiceNodes(dim) match {  
      
      case Nil => {
        // the choices get merged later on by ++
        val choice = new ChoiceNode(dim, 
      
          Map(tag -> nodes.flatMap {            
            // 1. All dimension nodes located at root
            case dim:DimensionNode => Set(dim)
        
            // 2. All dimension nodes nested inside of Choicenodes
            case ChoiceNode(_, edges) => edges.map(_._2).flatten
          })          
        )
  
        DimensionGraph(nodes.filter { _.isInstanceOf[ChoiceNode] } + choice)
      }
      
      case _ => {
        message(ast, "ERROR: Nested choices of the same dimension are not supported! (%s)".format(dim.name))
        return this
      }
    }
        
    
    def select(dim: Symbol, tag: Symbol)(ast: ASTNode): DimensionGraph = getDimensionNodes(dim) match {
    
      case Nil => {
        message(ast, "WARNING: your selection %s.%s is vacuous - maybe there is something bad going on".format(dim.name, tag.name))
        return this 
      }
    
      case dimension :: _ => {
                
        if (!(dimension.tags contains tag))
          message(ast, "ERROR: cannot select %s.%s because this tag is not declared".format(dim.name, tag.name))
      
        if (!(dimension.edges contains tag))
          message(ast, "ERROR: cannot select %s.%s because the choice for this tag is missing".format(dim.name, tag.name))         
          
        val newNodes = nodes - dimension
          
        // here only those dimensions are added, which are not dependent anymore
        // we gracefully fall back to Set(), if the tag is not found
        DimensionGraph(newNodes ++ dimension.edges.getOrElse(tag, Set()).filterNot( isDependent(newNodes, _) ))    
      }
    }
    
    
    /**
     * 1. We have to assure, that merging
     * 
     *   (dim A<...> in ...) + (dim A<...> in ...)
     * 
     * fails. If they are dependent, it should not be a problem
     */
    def merge(other: DimensionGraph)(ast: ASTNode): DimensionGraph = {
      
      val newNodes = other.nodes.collect[GraphNode, Set[GraphNode]] {
      
        // 1. If two DimensionNodes with the same name are declared -> Throw an error
        //    otherwise add dimension to the set of nodes
        case dim@DimensionNode(name, _, _) => getDimensionNodes(name) match {
          case Nil => dim
          case _ => {
            message(ast, "ERROR: Multiple dimension declarations at one level: %s".format(dim))
            dim
          }
        }
          
        // 2. If two ChoiceNodes with the same name are declared -> Merge their edges 
        case choice@ChoiceNode(name, edges) => getChoiceNodes(name) match {
          case Nil => choice
          case (otherChoice:ChoiceNode) :: Nil => new ChoiceNode(name, choice.edges ++ otherChoice.edges.collect {
            case (tag, targets) if choice.edges contains tag => (tag, choice.edges(tag) ++ targets)
            case other => other
          })
        }    
      
      // TODO clean up this part - much redundancy here
      } ++ nodes.collect {
        case dim@DimensionNode(name, _, _) if other.getDimensionNodes(name).isEmpty => dim
        case choice@ChoiceNode(name, _) if other.getChoiceNodes(name).isEmpty => choice
      }
    
      DimensionGraph(newNodes)
    }
  
    /**
     * Checks whether some other node points to this one.
     */
    protected def isDependent(nodes: Set[GraphNode], dim: GraphNode): Boolean = {
      for (node <- nodes; d <- node) {
        if (d == dim)
          return true
      }
      false
    }
    
    override def toString = 
      "Nodes:\n  %s\n".format(nodes mkString "\n  ")
    
  }
  object DimensionGraph {
    def apply(): DimensionGraph = DimensionGraph(Set())
    def empty = DimensionGraph(Set())
  }
  
}