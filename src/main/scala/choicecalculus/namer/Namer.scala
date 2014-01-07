package choicecalculus
package namer

import lang.ASTNode
import lang.choicecalculus.{ Identifier, Share }

import utility.AttributableRewriter.Term
import utility.Attribution.{ attr, paramAttr }

import utility.DebugRewriter.{ everywheretd, query }

/**
 * Use kiama.util.Environment to implement share and id
 * `Identifier` is a named entity.
 * See Oberon0.L0.NameAnalyzer
 *
 * Entity in kiama === symbol in scala compiler
 *
 *
 * we could introduce a separate namer phase in order to
 * check:
 * 1. that choices are bound to dimensions (this is currently checked by the dimension checker)
 * 2. that two equally named dimensions are introduced in the same scope
 *
 * Since the scopes of share and dimension names are different (are they?) and we might
 * want to be more flexible with the merging strategy of equally named dimensions
 * I think just using a namer phase for shares should be enough.
 *
 * What introduces the scope of a dimension name? A choice of a different dimension?
 * What if a dimension is selected away as in:
 *
 * dim A<a,b> in A<dim B<a,b> in B<1,2>, 3>
 */
trait Namer {

  // we trigger id resolution once since rewriting might destroy the parent chain
  // and the parent chain is just used to lookup identifiers
  //
  // After triggering the resolution in all follow up phases the binding instance
  // of an Identifier can be resolved by `id->bindingInstance`.
  def runNamer(ast: ASTNode): ast.type = { 
    everywheretd (forceNameResolution) (ast); 
    ast 
  }

  val bindingInstance: Identifier[ASTNode] => Option[Share[_,_]] = attr { 
    case p => p->bindingInstanceOf(p) 
  }

  /**
   * TODO raise error if result is `None`
   */
  private val forceNameResolution = query { 
    case id: Identifier[ASTNode] => id->bindingInstance 
  }

  private val bindingInstanceOf: Identifier[ASTNode] => ASTNode => Option[Share[_,_]] = paramAttr {
    case id@Identifier(name) => {
      case s@Share(`name`, _, _) => Some(s)
      case node if node.isRoot => None
      
      // If the parent is a share expression we have to check whether we are in the binding branch
      // of the share. Otherwise this would create circular bindings, much like a letrec.
      case node => node.parent match {
        case p@Share(_, `node`, _) => p.parent[ASTNode]->bindingInstanceOf(id)
        case otherParent: ASTNode => otherParent->bindingInstanceOf(id)
        case _ => None
      }
    }
  }

}