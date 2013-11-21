package choicecalculus
package recovery

import utility.setops._
import utility.Node

import lang.ASTNode
import lang.choicecalculus.{ Choices, Choice }
import labeling.{ Path, Label }

trait CCRecovery extends PathRecovery with ChoiceRecovery with Dimensions {
  
  object problemGraph extends ProblemGraphBuilder[ASTNode]
  
  import problemGraph.NamedInstance
  
  def tableToGraph(table: CloneInstanceTable): ProblemGraph = 
    problemGraph.createFromTable(table)
  
  def graphToLabeledGraph(graph: ProblemGraph): (ProblemGraph, Map[Node, Label]) = 
    (graph, recover(graph))
    
  def labelsToChoices(graph: ProblemGraph, labels: Map[Node, Label]): Map[Symbol, ASTNode] = 
    for {
      (variable, nodes) <- graph.variables
      labelsWithValues  = labels.filter { case (n, _) => nodes contains n }
                                .map { case (n@NamedInstance(_, value), l) => (value, l) }
      ccexpr <- toCC(labelsWithValues)
    } yield (variable, ccexpr)  
  
  def process(table: CloneInstanceTable): Map[Symbol, ASTNode] = 
    graphToLabeledGraph(tableToGraph(table)) match {
      case (graph, labels) => labelsToChoices(graph, labels)
    }
    
  
  def tableToTable(table: CloneInstanceTable): CloneInstanceTable =
    tableFromChoices(process(table))
  
  // TODO last step before results can be checked automatically
  // REWRITE to use selection implementation, then generate all possible
  //  selections and create table after reduction
  def tableFromChoices(choices: Map[Symbol, ASTNode]): CloneInstanceTable = {
        
    val dims = choices.values.flatMap(collectDims).toMap

    // make sure that keys always occur in same order
    val keys = choices.keySet.toList
    
    val table = new CloneInstanceTable(keys:_*)
    
    if (dims.isEmpty)
      return table

    val selections: Set[Set[(Symbol, Symbol)]] = (for {
      (dim, tags) <- dims
    } yield tags.map( t => (dim, t) ).toSet).toSet
    
    val allSelections = selections.map(_.map(Set(_))).reduce[Set[Set[(Symbol, Symbol)]]](crossSets)
    
    for {
      sel <- allSelections
      row = keys.map { k => select(sel.toMap, choices(k)) }
    } table.addRow(row: _*)
    
    table
  }
  
  // this is a primitive implementation of choice selection
  private def select(selection: Map[Symbol, Symbol], expr: ASTNode): ASTNode = expr match {
    case Choices(dim, cases) => 
      cases.collectFirst { 
        case Choice(tag, v) if tag == selection(dim) => select(selection, v) 
      }.head
    case other => other
  }
  
  def crossSets[T](first: Set[Set[T]], second: Set[Set[T]]): Set[Set[T]] =
    for {
      el1 <- first
      el2 <- second
    } yield el1 ++ el2
  
  private def collectDims(expr: ASTNode): Map[Symbol, List[Symbol]] = expr match {
    case Choices(dim, cases) => 
      Map(dim -> cases.map(_.tag)) ++ cases.flatMap( c => collectDims(c.body) )
    case other => Map()
  } 
    
    
}
