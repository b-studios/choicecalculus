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
/*
object Main extends Parser with ParsingREPL[ASTNode] with Semantics {
  
  def start: Parser[ASTNode] = parser
  override def prompt = "> "
  
  object prettyprinter extends JavaScriptPP with ChoiceCalculusPP
    
  def process(tree: ASTNode) {
    
    println("Parsed: " + tree)
    
    
    val dims = tree->dimensioning
    
    report(emitter)
    val selected = rewrite (select) (tree)
    
    emitter.emitln("\n\nSelected: \n" + prettyprinter.pretty(prettyprinter.toDoc(selected)))
    
    //emitter.emitln("\n\nPrettyprinted: \n" + prettyPrinter.pretty(prettyPrinter.toDoc(tree)) )
  }
}*/


object CommandLine extends Compiler[ASTNode] 
    with Parser
    with Semantics {  
  
  object prettyprinter extends JavaScriptPP with ChoiceCalculusPP
  
  override def process(tree: ASTNode, console: Console, emitter: Emitter) : Boolean = {   
    
    val reduced = processTree(tree)
    
    emitter.emitln("dims: " + dimensioning(tree))
    
    report(emitter)
    
    emitter.emitln("Files: " + files)
    
    emitter.emitln(prettyprinter.pretty(prettyprinter.toDoc(reduced)) )
    
    true    
  } 
}
