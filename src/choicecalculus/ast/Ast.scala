package choicecalculus
package ast

import org.kiama.attribution.Attributable
import org.kiama.util.Positioned
import utility.Rebuildable

abstract class ASTNode extends Attributable with Positioned

/**
 * Expressions
 * -----------
 */
abstract class Expression extends ASTNode

/**
 * Host Language Expressions
 */
abstract class HostLanguageExpression extends Expression with Rebuildable
abstract class ConstantExpr extends HostLanguageExpression
abstract class UnaryExpr extends HostLanguageExpression {
  def content: Expression
}
object UnaryExpr {
  def unapply(e: UnaryExpr): Option[Expression] = Some( e.content )
}
abstract class BinaryExpr extends HostLanguageExpression {
  def lhs: Expression
  def rhs: Expression
}
object BinaryExpr {
  def unapply(e: BinaryExpr): Option[(Expression, Expression)] = Some( (e.lhs, e.rhs) )
}

case class Num(value: Int) extends ConstantExpr
case class Add(lhs: Expression, rhs: Expression) extends BinaryExpr
case class Mul(lhs: Expression, rhs: Expression) extends BinaryExpr
case class FunctionExpr(content: Expression) extends UnaryExpr
case class GroupExpr(content: Expression) extends UnaryExpr


/**
 * Expressions of the Choice Calculus (CC)
 */
abstract class CCExpression extends Expression
case class DimensionExpr(name: Symbol, tags: List[Symbol], body: Expression) extends CCExpression

case class ChoiceExpr(dim: Symbol, choices: List[Choice]) extends CCExpression

// a single choice is a mapping from tag to host language expression - can only occur as immediate
// child of choice-expressions
case class Choice(tag: Symbol, body: Expression) extends ASTNode

case class SelectExpr(dim: Symbol, tag: Symbol, body: Expression) extends CCExpression

case class ShareExpr(name: Symbol, exp: Expression, body: Expression) extends CCExpression

case class IdExpr(name: Symbol) extends CCExpression

case class IncludeExpr(filename: String) extends CCExpression


/** Nodes needed for implementation purpose only */
case class PartialConfig(body: Expression, selections: List[(Symbol, Symbol)]) extends CCExpression