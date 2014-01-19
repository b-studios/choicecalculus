package choicecalculus
package lang.javascript
package trees

import lang.trees.Tree

case class Program(contents: List[Tree]) extends Tree

trait Statement extends Tree
trait Expression extends Statement
trait Literal extends Expression

case class VarDeclStmt(bindings: List[Tree]) extends Statement
case class VarBinding(name: Tree, binding: Tree) extends Tree
case class BlockStmt(stmts: List[Tree]) extends Statement
case class IfStmt(cond: Tree, thenBlock: Tree, elseBlock: Option[Tree]) extends Statement
case class WhileStmt(cond: Tree, body: Tree) extends Statement
case class DoWhileStmt(body: Tree, cond: Tree) extends Statement
case class ForStmt(init: Option[Tree], cond: Option[Tree], incr: Option[Tree], body: Tree) extends Statement
case class ForInStmt(init: Tree, collection: Tree, body: Tree) extends Statement
case class SwitchStmt(head: Tree, cases: List[Tree]) extends Statement

case class MatchingCase(matcher: Tree, body: List[Tree]) extends Tree
case class DefaultCase(body: List[Tree]) extends Tree
case class BreakStmt(label: Option[Tree]) extends Statement
case class ContinueStmt(label: Option[Tree]) extends Statement
case class ThrowStmt(body: Tree) extends Statement

case class TryStmt(body: Tree, catchBlock: Option[Tree], finallyBlock: Option[Tree]) extends Statement
case class CatchBlock(name: Tree, body: Tree) extends Tree
case class FinallyBlock(body: Tree) extends Tree

case class ReturnStmt(body: Option[Tree]) extends Statement
case class WithStmt(binding: Tree, body: Tree) extends Statement
case class LabeledStmt(label: Tree, body: Tree) extends Statement
case object EmptyStmt extends Statement

case class AtomLit(text: String) extends Literal

case class BinaryOpExpr(lhs: Tree, op: String, rhs: Tree) extends Expression
case class TernaryExpr(cond: Tree, trueBlock: Tree, falseBlock: Tree) extends Expression
case class PrefixExpr(op: String, body: Tree) extends Expression
case class PostfixExpr(body: Tree, op: String) extends Expression
case class NewExpr(body: Tree) extends Expression
case class CallExpr(body: Tree, args: List[Tree]) extends Expression
case class MemberExpr(body: Tree, access: Tree) extends Expression
case class NameAccessExpr(body: Tree, name: Tree) extends Expression
case class ArrayExpr(contents: List[Tree]) extends Expression
case class ObjectExpr(bindings: List[Tree]) extends Expression

case class GroupExpr(content: Tree) extends Expression
case class SequenceExpr(contents: List[Tree]) extends Expression

case class PropertyBinding(name: Tree, value: Tree) extends Tree

case class FunctionDecl(name: Tree, args: List[Tree], body: Tree) extends Expression
case class FunctionExpr(args: List[Tree], body: Tree) extends Expression