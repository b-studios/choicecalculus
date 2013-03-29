package choicecalculus
package interpreter

import org.kiama.output.PrettyPrinter
import org.kiama.util.{ParsingREPL, Emitter, Compiler, Console}
import org.kiama.attribution.Attribution.initTree
import ast.{ ASTNode, ChoiceCalcPP, JavaScriptPP }
import parser.Parser

import semantics.{ TypeSystemRevised, DimensionGraph }

object Main extends Parser with ParsingREPL[ASTNode] with TypeSystemRevised with DimensionGraph {
  
  def start: Parser[ASTNode] = parser
  override def prompt = "> "
    
  def process(tree: ASTNode) {
    
    println("Parsed: " + tree)
    
  }
}
// with DimensionChecker with ChoiceGraph
object CommandLine extends Compiler[ASTNode] 
    with Parser
    with TypeSystemRevised
    with DimensionGraph {  
  
  import org.kiama.util.Messaging.{messagecount, report}
  import org.kiama.rewriting.Rewriter.{rewrite}
  
  override def process(ast: ASTNode, console: Console, emitter: Emitter) : Boolean = {
    super.process(ast, console, emitter)
    
    emitter.emitln("Parsed: " + ast)

    
    object prettyPrinter extends JavaScriptPP
    
    emitter.emitln("\n\nPrettyprinted: \n" + prettyPrinter.pretty(prettyPrinter.toDoc(ast)) )
    
    // currently just parse!
    /*
    val dims = ast->dimensioning
    
    report(emitter)
    
    // no error messages while semantic analysis
    emitter.emitln("Dimensions: " + dims)
    
    val selected = rewrite (selectRelation) (ast)
    
    emitter.emitln("After performing selection:")
    emitter.emitln( pretty(toDoc(selected)) )
    
    initTree(selected)
    
    val substituted = rewrite (performSubstitution) (selected)
    
    emitter.emitln("After substituting shares:")
    emitter.emitln( pretty(toDoc(substituted)) )
    */
    true
    
  }
  
  /*
  val contents = io.Source.fromFile(args(0), "utf-8").getLines.mkString("\n")
  
  parseAll(parser, contents) match {
    case Success(tree, _) => {
      val dims = dimensionOf(tree, Map.empty)
      println("Parse Tree :" + tree)
      println("Dimensions: " + dims)
      
      // If a choicegraph can be created (and it is cyclefree, then we can be sure, that there are no
      // dependent choices which cannot be resolved (and no inner choices can be selected without selecting the outer
      // ones first. (Addressing Problem (4) )
      //println("Choicegraph: " + getChoiceGraph(tree))
      
      //println("Reduced: " + myEval(tree, Map()))
    }
    case fail: NoSuccess => sys error fail.msg
  }
  */
  
  
}