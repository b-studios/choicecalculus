package choicecalculus
package semantics

import ast.{ ASTNode, DimensionExpr, SelectExpr, ChoiceExpr, IdExpr, Choice, ShareExpr, PartialConfig, IncludeExpr }
import utility.AttributableRewriter.{ all, bottomup, reduce, rewrite, rule, sometd }

trait Selecting { self: Choosing => 
  
  // We do the traversal bottomup to perform inner selection first
  val select = bottomup (reduce (selectRelation))
  
  val selectRelation = rule {
    
    // Don't select dependent dimensions!
    case SelectExpr(_, _, c:ChoiceExpr[_]) => c
    
    case SelectExpr(dim, tag, DimensionExpr(name, tags, body)) if name == dim =>
      rewrite (choose(dim, tag)) (body)
  
    case SelectExpr(dim, tag, id:IdExpr[_]) => PartialConfig(id, List((dim, tag)))
    
    case SelectExpr(dim, tag, inc:IncludeExpr[_,_]) => PartialConfig(inc, List((dim, tag)))
  
    case SelectExpr(dim, tag, PartialConfig(body, configs)) => PartialConfig(body, configs ++ List((dim, tag)))
    
    case SelectExpr(dim, tag, ShareExpr(name, expr, body)) => 
      ShareExpr(name, expr, SelectExpr(dim, tag, body))
      
    // TODO does this make sense? could help to be independend of bottomup/topdown
    case SelectExpr(dim, tag, inner@SelectExpr(dim2, tag2, body)) if dim == dim2 => inner
  
    case SelectExpr(dim, tag, DimensionExpr(name, tags, body)) if name != dim => 
      DimensionExpr(name, tags, SelectExpr(dim, tag, body)) 
  
    // Hostlanguage constructs - wrap every child into selectexpressions and reconstruct node
    case SelectExpr(dim, tag, t) => rewrite ( all ( rule {
      case n:ASTNode => SelectExpr(dim, tag, n)
      case l:Seq[ASTNode] => l.map(SelectExpr(dim, tag, _))
      case lit => lit
    })) (t)
  }
}


/**
 * In this relation we use function definition instead of syntactic representation to omit introduction of yet
 * another syntactic element. Can easily be rewritten.
 * 
 * Since the congruence rules match completely kiama's default behavior we can use `sometd(rule {...})` instead
 * of implementing all congruences by ourselves. `sometd` will try to continue on subtrees until it successfully 
 * matches a rule. Then it will stop processing this subtree.
 */
trait Choosing {
 
  def choose(dim: Symbol, tag: Symbol) = sometd ( rule {
    
    // select expressions shadow other ones, with the same dimension
    case e@SelectExpr(d, _, _) if d == dim => e
    
    // selection stops at dimensions with the same name
    case e@DimensionExpr(d, _, _) if d == dim => e
    
    // actual choosing 
    case ChoiceExpr(d, choices) if d == dim => choices.collect {
      case Choice(t, body) if t == tag => body
    }.head
    
  })
 
}