package choicecalculus.lang

import org.kiama.output.ParenPrettyPrinter

trait PrettyPrinter extends ParenPrettyPrinter with org.kiama.output.PrettyPrinter  {
  
  def toDoc(e: ASTNode): Doc = sys error "Implement toDoc"
  
}