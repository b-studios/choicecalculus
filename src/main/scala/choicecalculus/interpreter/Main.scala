package choicecalculus
package interpreter

import org.kiama.util.{ ParsingREPL, Emitter, Compiler, Console }
import lang.ASTNode
import lang.choicecalculus.ChoiceCalculusParser
import lang.prettyprinter._

import semantics.Semantics

import utility.Attribution
import utility.Attribution.initTree
import utility.Messaging.{ messagecount, report }
import utility.DebugRewriter.{ rewrite, debugging }

object CommandLine extends Compiler[ASTNode] 
    with ChoiceCalculusParser
    with Semantics {  
  
  override def process(filename: String, tree: ASTNode, console: Console, emitter: Emitter) : Boolean = {   
  
    val result = processTree(tree) 
    report(emitter)
    
    result.fold(emitter.emitln, (rewrite) => {          
        emitter.emitln(rewrite.pretty)
        emitter.emitln(dimensioning(rewrite))
      }
    )
    
    true    
  } 
}
