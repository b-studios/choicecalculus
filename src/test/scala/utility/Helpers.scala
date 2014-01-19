package choicecalculus
package utility.test

import lang.trees._                             
import lang.javascript.trees.{ AtomLit, BinaryOpExpr, Expression }

import utility.messages.{ hasBeenReported, Level }

trait Helpers {

  /**
   * Constructor helpers
   */
  def dim(name: Symbol, tags: List[Symbol], body: Tree) =
    Dimension(name, tags, body)

  def dim(name: Symbol)(tags: Symbol*)(body: Tree) =
    Dimension(name, tags.toList, body)

  def choice(dim: Symbol, alts: List[Alternative]) =
    Choice(dim, alts)

  def choice(dim: Symbol)(alts: (Symbol, Tree)*) =
    Choice(dim, alts.map {
      case (tag, body) => Alternative(tag, body)
    }.toList)

  def alternative(tag: Symbol, body: Tree) =
    Alternative(tag, body)

  def select(dim: Symbol, tag: Symbol, body: Tree) =
    Select(dim, tag, body)

  def share(name: Symbol, exp: Tree, body: Tree) =
    Share(name, exp, body)

  def id(name: Symbol) =
    Identifier(name)

  def include(filename: String, context: AnyRef) =
    Include(filename, context)

  def partialConfig(selects: (Symbol, Symbol)*)(body: Tree) =
    PartialConfig(body, selects.toList)

  def lit(name: String) =
    AtomLit(name)

  implicit class BinOpConstructor(lhs: Tree) {
    def +(rhs: Tree) = BinaryOpExpr(lhs, "+", rhs)
    def *(rhs: Tree) = BinaryOpExpr(lhs, "*", rhs)
  }

  def vacuousWarning = hasBeenReported { msg =>
    msg.phase == 'dimensionchecker &&
      msg.level == Level.Warn &&
      (msg.message contains "vacuous")
  }

  def dependentWarning = hasBeenReported { msg =>
    msg.phase == 'dimensionchecker &&
      msg.level == Level.Warn &&
      (msg.message contains "dependent")
  }

  implicit def intToLit(n: Int): Tree = AtomLit(n.toString)
  implicit def stringToLit(s: String): Tree = AtomLit(s)
}
