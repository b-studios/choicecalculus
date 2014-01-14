package choicecalculus
package phases

import lang.ASTNode
import lang.choicecalculus.{ Identifier, Share, Choice, Dimension }

import org.kiama.attribution.Attribution.{ attr, paramAttr }
import org.kiama.rewriting.Rewriter

import scala.collection.mutable

import utility.messages._

import utility.IdentityHashMap

/**
 * <h2> The Namer phase
 */
trait Namer { self: Rewriter =>

  /**
   * Decorates the given tree with references from variables to their
   * binding instances.
   *
   * It should be used before any rewriting takes places since rewriting
   * might destroy the parent chain which is essential to lookup identifiers.
   *
   * After triggering the resolution, in all follow up phases the binding instance
   * of an `Identifier` can be resolved by `id->bindingInstance`.
   *
   * @see [[bindingInstance]]
   */
  def runNamer(ast: ASTNode): ASTNode = messageScope(phase = 'namer) {
    everywheretd(forceNameResolution)(ast);
    ast
  }

  /**
   * Resolves the binding of the given `Identifier`
   *
   * @example {{{
   *   val id = Identifier('x)
   *   val tree = Share('x, ..., Add(id, Numeral(4)))
   *   initTree(tree)
   *   id->bindingInstance // => tree
   * }}}
   *
   * @param identifier to resolve the binding from
   * @return the share expression that binds the `Identifier` it is called on
   *         or a FatalPhaseError if the identifier is unbound
   */
  lazy val symbol: ASTNode => Sym = { node =>
    symbolTable getOrElseUpdate(node, node match {
      case (_:Dimension[_] | _:Share[_,_]) =>
        symbolTable getOrElseUpdate(node, Sym(node))
      case _ => node->symbolFor(node)
    })
  }

  lazy val copySymbol: ASTNode => ASTNode => ASTNode  = { to => from =>
    // println(s"copy symbol called \n  $from \n  $to")
    symbolTable get(from) map { symbolTable update(to, _) }
    to
  }

  lazy val moveSymbolTo: ASTNode => ASTNode => ASTNode  = { to => from =>
    // println(s"move symbol called \n$from \n $to")
    symbolTable get(from) map { sym =>
      sym.definition = to
      symbolTable update(to, sym)
    }
    to
  }

  lazy val moveSymbolFrom: ASTNode => ASTNode => ASTNode  = { from => to =>
    from->moveSymbolTo(to)
    to
  }

  /**
   * Called Sym since otherwise interfers with scala builtin Symbol
   */
  case class Sym(var definition: ASTNode)

  private val symbolTable: mutable.Map[ASTNode, Sym] =
    new IdentityHashMap[ASTNode, Sym]


  private lazy val forceNameResolution = query {
    case id: Identifier[ASTNode] => id->symbol
    case choice: Choice[ASTNode] => choice->symbol
  }

  private def unbound(node: ASTNode): Nothing = node match {
    case id: Identifier[_] =>
      raise(s"Use of unbound choice calculus variable '${id.name.name}'", position = id)
    case choice: Choice[_] =>
      raise(s"Choice is not bound by a surrounding dimension '${choice.dim.name}'", position = choice)
  }

  private lazy val symbolFor: ASTNode => ASTNode => Sym =
    paramAttr {
      // scoping rules for `share`
      case id @ Identifier(name) => {
        case s @ Share(`name`, _, _) => symbolTable getOrElseUpdate(s, Sym(s))
        case node if node.isRoot => unbound(id)

        case node => node.parent match {
          case p @ Share(_, `node`, _) => p.parent[ASTNode]->symbolFor(id)
          case otherParent: ASTNode => otherParent->symbolFor(id)
          case _ => unbound(id)
        }
      }

      // scoping rules for `dim`
      case choice @ Choice(dim, _) => {
        case d @ Dimension(`dim`, _, _) => symbolTable getOrElseUpdate(d, Sym(d))
        case node if node.isRoot => unbound(choice)
        case node => node.parent[ASTNode]->symbolFor(choice)
      }

      // else: fail
      case other => raise(s"Cannot get symbol for: $other", position = other)
    }
}