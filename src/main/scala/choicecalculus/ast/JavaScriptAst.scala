package choicecalculus
package ast

case class Program(contents: List[Statement]) extends HostLanguageNode

trait Statement extends HostLanguageNode
case class VarDeclStmt(bindings: List[VarBinding]) extends Statement
case class VarBinding(name: Literal, binding: Expression) extends HostLanguageNode
case class BlockStmt(stmts: List[Statement]) extends Statement
case class IfStmt(cond: Expression, thenBlock: Statement, elseBlock: Option[Statement]) extends Statement
case class WhileStmt(cond: Expression, body: Statement) extends Statement
case class DoWhileStmt(body: Statement, cond: Expression) extends Statement
case class ForStmt(init: Option[Statement], cond: Option[Expression], incr: Option[Expression], body: Statement) extends Statement
case class ForInStmt(init: Statement, collection: Expression, body: Statement) extends Statement
case class SwitchStmt(head: Expression, cases: List[SwitchCase]) extends Statement
abstract class SwitchCase extends HostLanguageNode {
  def body: List[Statement]
}
case class MatchingCase(matcher: Expression, body: List[Statement]) extends SwitchCase
case class DefaultCase(body: List[Statement]) extends SwitchCase
case class BreakStmt(label: Option[Literal]) extends Statement
case class ContinueStmt(label: Option[Literal]) extends Statement
case class ThrowStmt(body: Expression) extends Statement

case class TryStmt(body: BlockStmt, catchBlock: Option[CatchBlock], finallyBlock: Option[FinallyBlock]) extends Statement
case class CatchBlock(name: Literal, body: Statement) extends HostLanguageNode
case class FinallyBlock(body: Statement) extends HostLanguageNode

case class ReturnStmt(body: Option[Expression]) extends Statement
case class WithStmt(binding: Expression, body: Statement) extends Statement
case class LabeledStmt(label: Literal, body: Statement) extends Statement
case object EmptyStmt extends Statement

trait Expression extends Statement

case class Literal(text: String) extends Expression

case class BinaryOpExpr(lhs: Expression, op: String, rhs: Expression) extends Expression
case class TernaryExpr(cond: Expression, trueBlock: Expression, falseBlock: Expression) extends Expression
case class PrefixExpr(op: String, body: Expression) extends Expression
case class PostfixExpr(body: Expression, op: String) extends Expression
case class NewExpr(body: Expression) extends Expression
case class CallExpr(body: Expression, args: List[Expression]) extends Expression
case class MemberExpr(body: Expression, access: Expression) extends Expression
case class NameAccessExpr(body: Expression, name: Literal) extends Expression
case class ArrayExpr(contents: List[Expression]) extends Expression
case class GroupExpr(content: Expression) extends Expression
case class SequenceExpr(contents: List[Expression]) extends Expression

case class ObjectExpr(bindings: List[PropertyBinding]) extends Expression

abstract class Binding extends HostLanguageNode {
  def name: Literal
}
case class PropertyBinding(name: Literal, value: Expression) extends Binding
//case class AccessMethodBinding(accessType: String, name: String, )

case class FunctionDecl(name: Literal, args: List[Literal], body: BlockStmt) extends Expression
case class FunctionExpr(args: List[Literal], body: BlockStmt) extends Expression