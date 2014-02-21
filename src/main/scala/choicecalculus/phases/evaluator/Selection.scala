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

    case Select(dim, tag, Dimension(name, tags, body)) if name == dim => Choose(dim, tag, body)
      //rewrite(choose(dim, tag))(body)

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
    // over another (or over a `choose`) - similar to the rules
    //     case Select(_, _, Select(_, _, _)) => SKIP
    //     case Select(_, _, Choose(_, _, _)) => SKIP
    case s @ Select(dim, tag, t) if !t.isInstanceOf[Choose] && !t.isInstanceOf[Select] => rewrite(all(rule {
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

    // selection stops at dimensions with the same name since all choices
    // in body are lexically bound by this dimension
    case Choose(dim, tag, d @ Dimension(name, tags, body)) if dim == name => d

    // actual choosing of an alternative
    case Choose(dim, tag, Choice(d, alts)) if d == dim => alts.collect {
      case Alternative(t, body) if t == tag => Choose(dim, tag, body)
    }.head

    case Choose(dim, tag, c @ Choice(d, alts)) if d != dim => Choice(d, alts.map {
      case Alternative(t, body) => Alternative(t, Choose(dim, tag, body))
    })->copySymbolFrom(c)

    case Choose(dim, tag, p @ PartialConfig(body, configs)) => p

    // choose must not jump over other chooses or selects (see #9)
    case c @ Choose(dim, tag, t) if !t.isInstanceOf[Choose] && !t.isInstanceOf[Select] => rewrite(all(rule {
      case n: Tree => Choose(dim, tag, n)
      case l: Seq[_] => l.map {
        case node: Tree => Choose(dim, tag, node)
        case other => other
      }
      case o: Option[_] => o.map { 
        case node: Tree => Choose(dim, tag, node)
        case other => other
      }
      case other => other
    }))(t)
  })
}