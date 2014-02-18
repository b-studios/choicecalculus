package choicecalculus
package phases
package evaluator

import lang.trees._

import org.kiama.rewriting.{ Rewriter, Strategy }

trait Selection { self: Namer with Rewriter =>

  lazy val select: Strategy = rule("selectRelation", {

    // Don't select dependent dimensions!
    // This is ruled out by the dimension checker...
    case Select(_, _, c: Choice) => c

    case Select(dim, tag, Dimension(name, tags, body)) if name == dim =>
      rewrite(choose(dim, tag))(body)

    case Select(dim, tag, id: Identifier) => PartialConfig(id, List((dim, tag)))

    case Select(dim, tag, inc: Include) => PartialConfig(inc, List((dim, tag)))

    case Select(dim, tag, PartialConfig(body, configs)) => 
      PartialConfig(body, configs ++ List((dim, tag)))

    case Select(dim, tag, old @ Share(name, expr, body)) =>
      Share(name, expr, Select(dim, tag, body))->moveSymbolFrom(old)

    case Select(dim, tag, old @ Dimension(name, tags, body)) if name != dim =>
      Dimension(name, tags, Select(dim, tag, body))->moveSymbolFrom(old)

    // Hostlanguage constructs - wrap every child into selectexpressions and 
    // reconstruct node the instanceOf check is to prevent one select "jumping" 
    // over another - similar to the rule
    //     case SelectExpr(_, _, SelectExpr(_, _, _)) => SKIP
    case s @ Select(dim, tag, t) if !t.isInstanceOf[Select] => rewrite(all(rule {
      case n: Tree => Select(dim, tag, n)
      case l: Seq[_] => l.map {
        case node: Tree => Select(dim, tag, node)
        case other => other
      }
      case o: Option[_] => o.map { 
        case node: Tree => Select(dim, tag, node)
        case other => other
      }
      case other => other
    }))(t)
  })

  /**
   * In this relation we use function definition instead of syntactic 
   * representation to omit introduction of yet another syntactic element (e.g. 
   * "pushing down" `Chosen('A, 'a, body)`. Can easily be rewritten.
   *
   * `sometd` will try to continue on subtrees until it successfully matches a 
   * rule. Then it will stop processing this subtree.
   */
  def choose(dim: Symbol, tag: Symbol): Strategy = sometd(rule("choose", {

    // select expressions shadow other ones, with the same dimension
    case e @ Select(d, _, _) if d == dim => e

    // selection stops at dimensions with the same name
    case e @ Dimension(d, _, _) if d == dim => e

    // actual choosing of an alternative
    case Choice(d, alts) if d == dim => alts.collect {
      case Alternative(t, body) if t == tag => rewrite(choose(dim, tag))(body)
    }.head

  }))
}