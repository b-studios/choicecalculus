package choicecalculus
package recovery

import utility.{ ColoringBruteforceSolver, Node }

import labeling.{ Path, Label }

private[recovery] trait PathRecovery { self: Dimensions =>
  
  // Optimization step: split into connected components (this reduces the amount
  // of instantiations per variable)
  def recover(graph: ProblemGraph): Map[Node, Label] = {
    
    import graph.{ nodes, edges, variables }
    
    // initialization
    val labels = nodes.map { (_, Label()) }.toMap
        
    val solver = new ColoringBruteforceSolver[Node] {
      def connected(n1: Node, n2: Node): Boolean =
        !(labels(n1) disjoint labels(n2))
    }
    
    var alreadyHandled: Set[Node] = Set.empty
    
    for (group <- variables.values.toList.sortBy { _.size }) {
      val solution = solver.solve(group)
      
      // not already disjoint
      if (solution.size > 1) {
        val (dim, tags) = createDimension(solution.size)
        
        for ((solgroup, tag) <- solution.zip(tags); node <- solgroup) {
          
          // just add suffix to those paths that need disambiguation!
          // TODO move this into Label implementation
          val l@Label(paths) = labels(node)
          l.paths = paths.map {
            case p if !(group - node).forall { labels(_) disjoint Label(Set(p)) } =>
              p:+(dim, tag)
            case p => p
          }
          
          alreadyHandled += node
          
          for {
            target <- nodes;
            if !(alreadyHandled contains target)
            if solgroup.exists { src =>
              // Only propagate if label has been attached to source and
              // source is connected with target
              labels(src).containsChoiceFor(dim) && edges.exists { _.connects(src, target) }
            }
            
            // TODO check whether this makes sense
            //   only propagate labels if target's variable is not already disjoint
            targetVar <- variables.values.find { _ contains target }
            if !targetVar.forall { o => o == target || labels(o).disjoint(labels(target)) }
          } labels(target).addSuffix(dim, tag)
        }
      }
      
      // println(labels.map { case (n, l) => n.name.name + ": " + l }.mkString("\n"))
    }
    
    // NOTE: turned off path compression since it does not work on Cai's example

    // for (label <- labels.values)  {
    //   allVariantsStripping(label)
    // }

    //println("\nafter cleanup")
    //println(labels.map { case (n, l) => n.name.name + ": " + l }.mkString("\n"))
    
    labels
  }

   /**
   * All variants stripping ({ αA1β, ..., αAkβ } -> {αβ} if A<1,...,k> is defined)
   * Complexity: |dims| * |paths|
   */
  private[recovery] def allVariantsStripping(label: Label): Unit = 
    for ((dim, tags) <- dimensions) {
    
      label.paths.map { _.replaceByDummy(dim) }
        .groupBy { case (tag, restPath) => restPath }
        .mapValues { _.collect { case (Some(tag), path) => tag }.toSet }
        .collect {
          case (path, tags) if (dimensions(dim).toSet == tags) =>            
            label.paths = label.paths -- path.replaceDummyByChoices(dim, tags)
            label.paths = label.paths + path.removeDummy
      }
  }
}