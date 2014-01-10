package choicecalculus
package phases

import lang.ASTNode
import lang.choicecalculus.{ Identifier, Share }

import org.kiama.attribution.Attribution.{ attr, paramAttr }
import org.kiama.rewriting.Rewriter.{ everywheretd, query }

import utility.messages._


/**
 * <h2> The Namer phase
 */
trait Namer {

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
    everywheretd (forceNameResolution) (ast);
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
  lazy val bindingInstance: Identifier[ASTNode] => Share[ASTNode, ASTNode] = attr { 
    case p => p->bindingInstanceOf(p)
  }

  private lazy val forceNameResolution = query { 
    case id: Identifier[ASTNode] => id->bindingInstance
  }

  private def unbound(id: Identifier[_]): Nothing =
    raise(s"Use of unbound choice calculus variable '${id.name.name}'", position = id)

  private lazy val bindingInstanceOf: Identifier[ASTNode] => ASTNode => Share[ASTNode,ASTNode] = 
    paramAttr {
      case id@Identifier(name) => {
        case s@Share(`name`, _, _) => s
        case node if node.isRoot => unbound(id)
        
        // If the parent is a share expression we have to check whether we are in the binding branch
        // of the share. Otherwise this would create circular bindings, much like a letrec.
        case node => node.parent match {
          case p@Share(_, `node`, _) => p.parent[ASTNode]->bindingInstanceOf(id)
          case otherParent: ASTNode => otherParent->bindingInstanceOf(id)
          case _ => unbound(id)
        }
      }
    }
}