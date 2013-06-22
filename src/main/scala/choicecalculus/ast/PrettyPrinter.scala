package choicecalculus
package ast

import org.kiama.output.ParenPrettyPrinter

trait PrettyPrinter extends ParenPrettyPrinter with org.kiama.output.PrettyPrinter  {
  
  def toDoc(e: ASTNode): Doc = sys error "Implement toDoc"
  
}

trait ChoiceCalculusPP extends PrettyPrinter {

  val cc_prefix = text("#")
  
  override def toDoc(e: ASTNode): Doc = e match {
    
    case DimensionExpr(dim, tags, body) => 
      "dim" <+> text(dim.name) <> parens(fillsep(tags.map( (t) => text(t.name)), comma)) <+> 
        "in" <+> toDoc(body)
    
    case ChoiceExpr(dim, choices) =>
      "choice" <+> text(dim.name) <+> 
        braces (nest (line <> ssep (choices.map(toDoc), line)) <> line)
    
    case Choice(tag, body) =>
      "case" <+> text(tag.name) <+> "=>" <+> toDoc(body)
      
    case SelectExpr(dim, tag, body) =>
      "select" <+> text(dim.name) <> dot <> text(tag.name) <+> "from" <+> toDoc(body)
      
    case IdExpr(id) =>
      cc_prefix <> text(id.name)
      
    case ShareExpr(x, binding, body) =>
      "share" <+> text(x.name) <+> equal <+> toDoc(binding) <+> "within" <+> toDoc(body)
    
    case PartialConfig(body, configs) =>      
      configs.foldLeft(toDoc(body)) {
        (old, config) => "select" <+> text(config._1.name) <> dot <> text(config._2.name) <+> "from" <+> old 
      }
      
    case IncludeExpr(filename, _) =>
      "include" <+> surround(filename, '"')
    
    case other => super.toDoc(other)     
  } 
}