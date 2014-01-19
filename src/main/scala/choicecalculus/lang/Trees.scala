package choicecalculus
package lang
package trees

import org.kiama.attribution.Attributable
import org.kiama.util.Positioned

trait Tree extends Attributable with Positioned

case class Dimension(name: Symbol, tags: List[Symbol], body: Tree) extends Tree

case class Choice(dim: Symbol, alternatives: List[Alternative]) extends Tree

case class Alternative(tag: Symbol, body: Tree) extends Tree

case class Select(dim: Symbol, tag: Symbol, body: Tree) extends Tree

case class Share(name: Symbol, exp: Tree, body: Tree) extends Tree

case class Identifier(name: Symbol) extends Tree

case class Include(filename: String, context: AnyRef) extends Tree

case class PartialConfig(body: Tree, selections: List[(Symbol, Symbol)]) extends Tree