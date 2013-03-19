package choicecalculus
package interpreter

import org.kiama.output.PrettyPrinter
import org.kiama.util.{ParsingREPL, Emitter}
import org.kiama.attribution.Attribution.initTree
import org.kiama.util.Messaging._
import ast.ASTNode
import parser.Parser
import semantics.{ DimensionChecker, ChoiceGraph }
import choicecalculus.semantics.ChoiceGraph
import choicecalculus.semantics.TypeSystem

object Main extends Parser with ParsingREPL[ASTNode] with TypeSystem {
  
  def start: Parser[ASTNode] = parser
  override def prompt = "> "
    
  def process(tree: ASTNode) {
    
    val dims = typeOf(tree, Map())
    println("Dimensions: " + dims)
    
    //println(tree)
  }
}
// with DimensionChecker with ChoiceGraph
object CommandLine extends App with Parser with TypeSystem {

  val contents = io.Source.fromFile(args(0), "utf-8").getLines.mkString("\n")
  
  parseAll(parser, contents) match {
    case Success(tree, _) => {
      val dims = typeOf(tree, Map())
      println("Parse Tree :" + tree)
      println("Dimensions: " + dims)
      
      // If a choicegraph can be created (and it is cyclefree, then we can be sure, that there are no
      // dependent choices which cannot be resolved (and no inner choices can be selected without selecting the outer
      // ones first. (Addressing Problem (4) )
      //println("Choicegraph: " + getChoiceGraph(tree))
      
      //println("Reduced: " + eval(tree))
    }
    case fail: NoSuccess => sys error fail.msg
  }
  
  
  
}