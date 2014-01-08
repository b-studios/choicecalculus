package choicecalculus
package phases.evaluator

import lang.ASTNode
import lang.choicecalculus.{ Choice, Alternative, Dimension, Include, 
                             PartialConfig, Select, Share, Identifier }

import org.kiama.rewriting.Rewriter.{ all, sometd, rewrite, rule }

import org.kiama.rewriting.Strategy

trait Selection {
  
  lazy val select: Strategy = rule("selectRelation", {
    
    // Don't select dependent dimensions!
    // This is ruled out by the dimension checker...
    case Select(_, _, c:Choice[_]) => c

    case Select(dim, tag, Dimension(name, tags, body)) if name == dim =>
      rewrite (choose(dim, tag)) (body)
  
    case Select(dim, tag, id:Identifier[_]) => PartialConfig(id, List((dim, tag)))
    
    case Select(dim, tag, inc:Include[_,_]) => PartialConfig(inc, List((dim, tag)))
  
    case Select(dim, tag, PartialConfig(body, configs)) => PartialConfig(body, configs ++ List((dim, tag)))
    
    case Select(dim, tag, Share(name, expr, body)) => 
      Share(name, expr, Select(dim, tag, body))
      
    case Select(dim, tag, Dimension(name, tags, body)) if name != dim => 
      Dimension(name, tags, Select(dim, tag, body))
      
    // Hostlanguage constructs - wrap every child into selectexpressions and reconstruct node
    // the instanceOf check is to prevent one select "jumping" over another - similar to the rule
    //     case SelectExpr(_, _, SelectExpr(_, _, _)) => SKIP
    case s@Select(dim, tag, t) if !t.isInstanceOf[Select[_]] => rewrite ( all ( rule {
      case n:ASTNode => Select(dim, tag, n)
      case l:Seq[ASTNode] => l.map(Select(dim, tag, _))
      case lit => lit
    })) (t)
  })


  /**
   * In this relation we use function definition instead of syntactic representation to omit introduction of yet
   * another syntactic element (e.g. "pushing down" `Chosen('A, 'a, body)`. Can easily be rewritten.
   * 
   * `sometd` will try to continue on subtrees until it successfully matches a rule. Then 
   * it will stop processing this subtree.
   */
  def choose(dim: Symbol, tag: Symbol) = sometd ( rule("choose", {
    
    // select expressions shadow other ones, with the same dimension
    case e@Select(d, _, _) if d == dim => e
    
    // selection stops at dimensions with the same name
    case e@Dimension(d, _, _) if d == dim => e
    
    // actual choosing of an alternative
    case Choice(d, alts) if d == dim => alts.collect {
      case Alternative(t, body) if t == tag => body
    }.head
    
  }))
}