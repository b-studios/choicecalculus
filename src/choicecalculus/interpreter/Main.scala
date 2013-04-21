package choicecalculus
package interpreter

import org.kiama.output.PrettyPrinter
import org.kiama.util.{ParsingREPL, Emitter, Compiler, Console}
import org.kiama.attribution.Attribution.initTree
import ast.{ ASTNode, ChoiceCalculusPP, JavaScriptPP }
import parser.Parser

import semantics.Semantics

import org.kiama.util.Messaging.{messagecount, report}
import org.kiama.rewriting.Rewriter.{rewrite}

object Main extends Parser with ParsingREPL[ASTNode] with Semantics {
  
  def start: Parser[ASTNode] = parser
  override def prompt = "> "
    
  def process(tree: ASTNode) {
    
    println("Parsed: " + tree)
    object prettyPrinter extends JavaScriptPP with ChoiceCalculusPP
    
    val dims = tree->dimensioning
    
    report(emitter)
    val selected = rewrite (select) (tree)
    
    emitter.emitln("\n\nSelected: \n" + prettyPrinter.pretty(prettyPrinter.toDoc(selected)))
    
    //emitter.emitln("\n\nPrettyprinted: \n" + prettyPrinter.pretty(prettyPrinter.toDoc(tree)) )
  }
}
// with DimensionChecker with ChoiceGraph
object CommandLine extends Compiler[ASTNode] 
    with Parser
    with Semantics {  
   
  override def process(tree: ASTNode, console: Console, emitter: Emitter) : Boolean = {   
    
    super.process(tree, console, emitter)

    object prettyPrinter extends JavaScriptPP with ChoiceCalculusPP
    
    emitter.emitln("\n\nPrettyprinted: \n" + prettyPrinter.pretty(prettyPrinter.toDoc(tree)) )
    
    // currently just parse!
    
    val dims = tree->dimensioning
    
    report(emitter)
    
    // no error messages while semantic analysis
    emitter.emitln("Dimensions: " + dims)
    
    
    val selected = rewrite (selectRelation) (tree)
    
    emitter.emitln("\n\nSelected: \n" + prettyPrinter.pretty(prettyPrinter.toDoc(selected)))
    
    /*
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
object A extends App {
 
  import org.kiama.attribution.Attribution.{attr, paramAttr, CachedParamAttribute}
  import org.kiama.rewriting.Rewriter._  
  
  
  abstract class Hostlang extends ASTNode
  case class BinaryOp(lhs: ASTNode, op: String, rhs: List[ASTNode]) extends ASTNode
  case class RewriteMe(content: String) extends ASTNode
  case object Literal extends ASTNode
  
  val tree = BinaryOp(
      BinaryOp(
        Literal,
        "*",
        List(RewriteMe("Hello1"), RewriteMe("Hello1"), RewriteMe("Hello1"))
      ),
      "+",
      List(BinaryOp(
        Literal,
        "*",
        List(RewriteMe("Hello2"))
      ))
  ) 
  
  val rewriteRule = rule {
    case RewriteMe("Hello1") => RewriteMe("Hello World")
  }
  
  val results = reduce (rewriteRule) (tree)
  
  println(results)
  
}
object B extends App with Semantics {
  
  import ast._
  import org.kiama.util.Messaging.{messagecount, report}
  import org.kiama.rewriting.Rewriter.{rewrite}
  
  val tree = Program(List(
    SelectExpr('foo, 'b, BinaryOpExpr(
      Literal("1234"),
      "+",
      DimensionExpr('foo, List('b, 'a), ChoiceExpr('foo, List(
        Choice('a, Literal("foo")),
        Choice('b, Literal("bar"))
      )))
    ))
  ))
  
  val selected = rewrite (selectRelation) (tree)
  
  println(selected)
  
}
