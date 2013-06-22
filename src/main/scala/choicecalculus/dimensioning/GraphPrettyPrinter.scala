package choicecalculus
package dimensioning

import org.kiama.output.ParenPrettyPrinter

trait GraphPrettyPrinter extends ParenPrettyPrinter with org.kiama.output.PrettyPrinter  {
    
  def pretty_graph(g: DimensionGraph): String = pretty(toDoc(g))
  
  def toDoc(g: DimensionGraph): Doc = nest(linebreak <> text("DimensionGraph") <+> braces(nest(line <> (
    ssep(g.nodes.map(toDoc).toSeq, line)
  )) <> line))
  
  def toDoc(edges: Map[Symbol, Set[GraphNode]]): Doc = braces(nest(line <>
      ssep( edges.map { 
        case (tag, target) => 
          text(tag.name) <> colon <+> braces ( ssep(target.map( (t) => space <> toDoc(t) ).toSeq, comma)) }.toSeq, 
       line)) <> line)
  
  def toDoc(e: GraphNode): Doc = e match {
    //"(%s) --%s--> %s".format(dim.name, edge._1, edge._2)
    case ChoiceNode(dim, edges) => 
      parens(text(dim.name)) <+> toDoc(edges)
    
    case DimensionNode(dim, tags, edges) => 
      text(dim.name) <> angles(ssep( tags.map { (t) => text(t.name) }, comma)) <+> toDoc(edges)
  }
  
}
object GraphPrettyPrinter extends GraphPrettyPrinter {}