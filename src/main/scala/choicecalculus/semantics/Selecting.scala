package choicecalculus
package semantics

import ast.{ ASTNode, DimensionExpr, SelectExpr, ChoiceExpr, IdExpr, Choice, ShareExpr, PartialConfig, IncludeExpr }
import utility.DebugRewriter.{ all, attr2attrFix, bottomup, congruence, debug, fail, reduce, rewrite, rule, sometd, topdown }
import org.kiama.rewriting.Strategy;

trait Selecting { self: Choosing => 
  
  lazy val select: Strategy = rule("selectRelation", {
    
    // Don't select dependent dimensions!
    case SelectExpr(_, _, c:ChoiceExpr[_]) => c
    
    case SelectExpr(dim, tag, DimensionExpr(name, tags, body)) if name == dim =>
      rewrite (choose(dim, tag)) (body)
  
    case SelectExpr(dim, tag, id:IdExpr[_]) => PartialConfig(id, List((dim, tag)))
    
    case SelectExpr(dim, tag, inc:IncludeExpr[_,_]) => PartialConfig(inc, List((dim, tag)))
  
    case SelectExpr(dim, tag, PartialConfig(body, configs)) => PartialConfig(body, configs ++ List((dim, tag)))
    
    case SelectExpr(dim, tag, ShareExpr(name, expr, body)) => 
      ShareExpr(name, expr, SelectExpr(dim, tag, body))
      
    case SelectExpr(dim, tag, DimensionExpr(name, tags, body)) if name != dim => 
      DimensionExpr(name, tags, SelectExpr(dim, tag, body))
      
    // Hostlanguage constructs - wrap every child into selectexpressions and reconstruct node
    // the instanceOf check is to prevent one select "jumping" over another - similar to the rule
    //     case SelectExpr(_, _, SelectExpr(_, _, _)) => SKIP
    case s@SelectExpr(dim, tag, t) if !t.isInstanceOf[SelectExpr[_]] => rewrite ( all ( rule {
      case n:ASTNode => SelectExpr(dim, tag, n)
      case l:Seq[ASTNode] => l.map(SelectExpr(dim, tag, _))
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
    case e@SelectExpr(d, _, _) if d == dim => e
    
    // selection stops at dimensions with the same name
    case e@DimensionExpr(d, _, _) if d == dim => e
    
    // actual choosing 
    case ChoiceExpr(d, choices) if d == dim => choices.collect {
      case Choice(t, body) if t == tag => body
    }.head
    
  }))
 
}