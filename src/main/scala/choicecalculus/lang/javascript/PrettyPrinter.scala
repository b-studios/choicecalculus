package choicecalculus
package lang.javascript

import lang.trees.Tree
import lang.javascript.trees._

import org.kiama.output.{ Infix, LeftAssoc, RightAssoc, NonAssoc, Fixity,
                          Prefix, Postfix }

trait PrettyPrinter extends lang.PrettyPrinter with utility.ParenPrettyPrinter {

  override val defaultIndent = 2
  override val defaultWidth = 80

  /**
   * Externalize the fixity and priority of binary and unary operators
   */
  def priorityOf(e: Tree): Int = e match {
    case _: SequenceExpr => 10
    case BinaryOpExpr(_, op ,_) if List(">>>=", ">>=", "+=", "-=", "*=", "/=",
      "%=" , "<<=", "^=", "&&=", "&=", "||=", "|=", "=") contains op => 20
    case _: TernaryExpr => 30
    case BinaryOpExpr(_, op ,_) if op == "||" => 40
    case BinaryOpExpr(_, op ,_) if op == "&&" => 50
    case BinaryOpExpr(_, op ,_) if op == "|"  => 60
    case BinaryOpExpr(_, op ,_) if op == "^"  => 70
    case BinaryOpExpr(_, op ,_) if op == "&"  => 80
    case BinaryOpExpr(_, op ,_) if List("===", "==", "!==", "!=") contains op => 90
    case BinaryOpExpr(_, op, _) if List(">=", ">", "<=", "<", "instanceof", 
      "in") contains op => 100
    case BinaryOpExpr(_, op, _) if List(">>>", ">>", "<<") contains op => 110
    case BinaryOpExpr(_, op, _) if List("+", "-") contains op => 120
    case _: BinaryOpExpr => 130
    case PrefixExpr(op, _) if List("!", "~", "+", "-", "void", "delete", 
        "typeof") contains op => 140
    case _: PrefixExpr  => 150
    case _: PostfixExpr => 160
    case _: NewExpr     => 170
    case _              => 200
  }

  def fixityOf(e: Tree): Fixity = e match {
    case (_: BinaryOpExpr) => Infix(LeftAssoc)
    case (_: PrefixExpr | _: NewExpr) => Prefix
    case _: PostfixExpr => Postfix
    case _: TernaryExpr => Infix(RightAssoc)
    case _ => Infix(NonAssoc)
  }
  def mightRequireParens(e: Tree): Boolean = e match {
    case (_: BinaryOpExpr | _: PrefixExpr | _: PostfixExpr 
         | _: TernaryExpr | _: NewExpr | _: SequenceExpr) => true
    case _ => false
  }

  /**
   * Places a space in front of the optional tree
   */
  def toDocOrEmpty(e: Option[Tree]) = e match {
    case Some(e) => space <> toDoc(e)
    case None => empty
  }

  def stmtsep(stmts: List[Tree]) = stmts match {
    case Nil => empty
    case (expr: Expression) :: Nil => exprInStmtPos(expr)
    case stmt :: Nil => toDoc(stmt)
    case many => many.init.foldRight(toDoc(many.last)) {
      // This is needed to avoid confusion when parsing object literals or
      // immediate function executions in statement position
      case (e: Expression, rest) => exprInStmtPos(e) <> semi <> linebreak <> rest
      case (s, rest) => toDoc(s) <> semi <> linebreak <> rest
    }
  }

  def nestIfNoBlock(e: Tree) = e match {
    case b: BlockStmt => toDoc(b)
    case other => nest(line <> toDoc(other)) <> linebreak
  }

  def forceBlock(e: Tree) = e match {
    case b: BlockStmt => toDoc(b)
    case _ => toDoc(BlockStmt(e :: Nil))
  }

  private def exprInStmtPos(e: Expression) = toDoc(e) match {
    // TODO This is REALLY expensive, but I currently don't know a better way
    // to inspect the document
    case doc if pretty(doc) startsWith "function " => parens(doc)
    case doc => doc
  }

  /**
   * Used to detect whether a prefix can be attached directly to 
   * it's base expression
   */
  private def conflictingPrefix(e: Tree): Boolean = e match {
    case PrefixExpr(op, _) if List("void", "delete", "typeof") contains op => true
    case PrefixExpr(opL, PrefixExpr(opR, _)) => {
      val (plus, minus) = (List("++", "+"), List("--", "-"))

      (plus contains opL) && (plus contains opR) ||
      (minus contains opL) && (minus contains opR)
    }
    case _ => false
  }

  private def conflictingPostfix(e: Tree): Boolean = e match {
    case PostfixExpr(PostfixExpr(_, opL), opR) => {
      val (plus, minus) = (List("++", "+"), List("--", "-"))

      (plus contains opL) && (plus contains opR) ||
      (minus contains opL) && (minus contains opR)
    }
    case _ => false
  }

  /**
   * Wraps terms into a block, using sep as separator between the
   * terms
   */
  private def block(terms: List[Doc], sep: Doc): Doc = terms match {
    case Nil => braces(empty)
    case _ => braces(nest(lsep(terms, sep)) <> line)
  }

  override def toDoc(e: Tree): Doc = e match {

    case AtomLit(contents) =>
      text(contents)

    case Program(contents) =>
      stmtsep(contents)

    case VarDeclStmt(bindings) =>
      "var" <+> nest(fillsep(bindings.map(toDoc), comma))

    case VarBinding(name, binding) => toDoc(name) <> (binding match {
      case AtomLit("undefined") => empty
      case other => space <> equal <+> toDoc(other)
    })

    case BlockStmt(stmts) => stmts match {
      case Nil => braces(empty)
      case _ => braces(nest(line <> stmtsep(stmts)) <> line)
    }

    case IfStmt(cond, thenBlock, elseBlock) =>
      "if" <+> parens(toDoc(cond)) <+> forceBlock(thenBlock) <> (elseBlock match {
        case Some(i: IfStmt) => space <> "else" <+> toDoc(i)
        case Some(e) => space <> "else" <+> forceBlock(e)
        case _ => empty
      })

    case WhileStmt(cond, body) =>
      "while" <+> parens(toDoc(cond)) <+> forceBlock(body)

    case DoWhileStmt(body, cond) =>
      "do" <+> forceBlock(body) <+> "while" <+> parens(toDoc(cond))

    case ForStmt(init, cond, incr, body) =>
      "for" <+> parens(toDocOrEmpty(init) <> semi <> toDocOrEmpty(cond) <> semi <> toDocOrEmpty(incr)) <+>
        forceBlock(body)

    case ForInStmt(init, collection, body) =>
      "for" <+> parens(toDoc(init) <+> "in" <+> toDoc(collection)) <+> forceBlock(body)

    case SwitchStmt(head, cases) =>
      "switch" <+> parens(toDoc(head)) <+> block(cases.map(toDoc), line)

    case MatchingCase(matcher, stmts) =>
      "case" <+> toDoc(matcher) <> ":" <> nest(line <> stmtsep(stmts))

    case DefaultCase(stmts) =>
      "default" <> ":" <> nest(line <> stmtsep(stmts))

    case BreakStmt(label) => "break" <> toDocOrEmpty(label)

    case ContinueStmt(label) =>
      "continue" <> toDocOrEmpty(label)

    case ThrowStmt(body) =>
      "throw" <+> toDoc(body)

    case TryStmt(body, catchBlock, finallyBlock) =>
      "try" <+> toDoc(body) <> toDocOrEmpty(catchBlock) <>
        toDocOrEmpty(finallyBlock)

    case CatchBlock(name, body) =>
      "catch" <+> parens(toDoc(name)) <+> toDoc(body)

    case FinallyBlock(body) =>
      "finally" <+> toDoc(body)

    case ReturnStmt(body) =>
      "return" <> toDocOrEmpty(body)

    case WithStmt(binding, body) =>
      "with" <+> parens(toDoc(binding)) <+> forceBlock(body)

    case LabeledStmt(label, body) =>
      toDoc(label) <> ":" <+> toDoc(body)

    case EmptyStmt => semi

    case BinaryOpExpr(l, op, r) => toParenDoc(e, l, r) { 
      case lhs :: rhs :: Nil => lhs <+> text(op) <+> rhs
    }

    case TernaryExpr(c, t, f) => toParenDoc(e, c, t, f) { 
      case cond :: trueBlock :: falseBlock :: Nil => 
        cond <+> question <+> trueBlock <+> colon <+> falseBlock
    }

    case PrefixExpr(op, b) => toParenDoc(e, b) { 
      case body :: Nil if (conflictingPrefix(e)) => text(op) <+> body
      case body :: Nil => text(op) <> body
    }

    case PostfixExpr(b, op) => toParenDoc(e, b) { 
      case body :: Nil if conflictingPostfix(e) => body <+> text(op)
      case body :: Nil => body <> text(op)
    }

    case NewExpr(b) => toParenDoc(e, b) {
      case body :: Nil => "new" <+> body
    }

    case CallExpr(b, args) => toParenDoc(e, b) {
      case body :: Nil => body <> parens(nest(fillsep(args.map(toDoc), comma)))
    }

    case MemberExpr(b, access) => toParenDoc(e, b) {
      case body :: Nil => body <> "[" <> toDoc(access) <> "]"
    }

    case NameAccessExpr(b, name) => toParenDoc(e, b) {
      case body :: Nil => body <> dot <> toDoc(name)
    }

    case ArrayExpr(contents) =>
      brackets(nest(fillsep(contents.map(toDoc), comma)))

    case GroupExpr(content) =>
      parens(toDoc(content))

    case SequenceExpr(contents) =>
      nest(fillsep(contents.map(toDoc), comma))

    case ObjectExpr(bindings) =>
      block(bindings.map(toDoc), comma)

    case PropertyBinding(name, value) =>
      toDoc(name) <> colon <+> toDoc(value)

    case FunctionDecl(name, args, body) =>
      "function" <+> toDoc(name) <> 
        parens(nest(fillsep(args.map(toDoc), comma))) <+> toDoc(body)

    case FunctionExpr(name, args, body) =>
      "function" <+> name.fold(empty)(n => toDoc(n) <> space) <>
        parens(nest(fillsep(args.map(toDoc), comma))) <+> toDoc(body)

  }
}
object PrettyPrinter extends PrettyPrinter {
  implicit val pp = this
}