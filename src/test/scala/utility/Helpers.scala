package choicecalculus
package utility.test

import lang.ASTNode
import lang.choicecalculus.{ Alternative, Choice, Dimension, Include,
                             PartialConfig, Select, Share, Identifier }
                             
import lang.javascript.{ AtomLit, BinaryOpExpr, Expression }

import utility.messages.{ hasBeenReported, Level }

trait Helpers {

  /**
   * Constructor helpers
   */
  def dim[T <: ASTNode](name: Symbol, tags: List[Symbol], body: T) =
    Dimension[T](name, tags, body)

  def dim[T <: ASTNode](name: Symbol)(tags: Symbol*)(body: T) =
    Dimension[T](name, tags.toList, body)

  def choice[T <: ASTNode](dim: Symbol, alts: List[Alternative[T]]) =
    Choice[T](dim, alts)

  def choice[T <: ASTNode](dim: Symbol)(alts: (Symbol, T)*) =
    Choice[T](dim, alts.map {
      case (tag, body) => Alternative(tag, body)
    }.toList)

  def alternative[T <: ASTNode](tag: Symbol, body: T) =
    Alternative[T](tag, body)

  def select[T <: ASTNode](dim: Symbol, tag: Symbol, body: T) =
    Select[T](dim, tag, body)

  def share[S <: ASTNode, T <: ASTNode](name: Symbol, exp: S, body: T) =
    Share[S, T](name, exp, body)

  def id[T <: ASTNode](name: Symbol) = Identifier[ASTNode](name)

  def include[T <: ASTNode, P](filename: String, context: P) =
    Include[T, P](filename, context)

  def partialConfig[T <: ASTNode](selects: (Symbol, Symbol)*)(body: T) =
    PartialConfig[T](body, selects.toList)

  def lit(name: String) = AtomLit(name)

  case class BinOpConstructor(lhs: Expression) {
    def +(rhs: Expression) = BinaryOpExpr(lhs, "+", rhs)
    def *(rhs: Expression) = BinaryOpExpr(lhs, "*", rhs)
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

  implicit def exp2binOp(e: Expression): BinOpConstructor = BinOpConstructor(e)
  implicit def intToLit(n: Int): AtomLit = AtomLit(n.toString)
}
