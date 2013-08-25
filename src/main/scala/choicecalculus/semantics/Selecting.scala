package choicecalculus
package semantics

import lang.ASTNode
import lang.choicecalculus.{ Choice, Choices, Dimension, Include, PartialConfig, Select, Share, SharedId }
import utility.DebugRewriter.{ all, attr2attrFix, bottomup, congruence, debug, fail, reduce, rewrite, rule, sometd, topdown }
import org.kiama.rewriting.Strategy;

trait Selecting { self: Choosing => 
  
  lazy val select: Strategy = rule("selectRelation", {
    
    // Don't select dependent dimensions!
    case Select(_, _, c:Choices[_]) => c
    
    case Select(dim, tag, Dimension(name, tags, body)) if name == dim =>
      rewrite (choose(dim, tag)) (body)
  
    case Select(dim, tag, id:SharedId[_]) => PartialConfig(id, List((dim, tag)))
    
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
}


/**
 * In this relation we use function definition instead of syntactic representation to omit introduction of yet
 * another syntactic element (e.g. "pushing down" `Chosen('A, 'a, body)`. Can easily be rewritten.
 * 
 * Since the congruence rules match completely kiama's default behavior we can use `sometd(rule {...})` instead
 * of implementing all congruences by ourselves. `sometd` will try to continue on subtrees until it successfully 
 * matches a rule. Then it will stop processing this subtree.
 */
trait Choosing {
 
  def choose(dim: Symbol, tag: Symbol) = sometd ( rule("chooseStep", {
    
    // select expressions shadow other ones, with the same dimension
    case e@Select(d, _, _) if d == dim => e
    
    // selection stops at dimensions with the same name
    case e@Dimension(d, _, _) if d == dim => e
    
    // actual choosing 
    case Choices(d, choices) if d == dim => choices.collect {
      case Choice(t, body) if t == tag => body
    }.head
    
  }))
 
}