package choicecalculus
package ast

import org.kiama.attribution.Attributable
import org.kiama.util.Positioned
import utility.Rebuildable

abstract class ASTNode extends Attributable with Positioned


/**
 * Host Language Expressions
 */
abstract class HostLanguageNode extends ASTNode

/**
 * Expressions of the Choice Calculus (CC)
 */
abstract class CCExpression extends Expression with Statement
case class DimensionExpr[T <: ASTNode](name: Symbol, tags: List[Symbol], body: T) extends CCExpression

case class ChoiceExpr[T <: ASTNode](dim: Symbol, choices: List[Choice[T]]) extends CCExpression

// a single choice is a mapping from tag to host language expression - can only occur as immediate
// child of choice-expressions
case class Choice[T <: ASTNode](tag: Symbol, body: T) extends ASTNode

case class SelectExpr[T <: ASTNode](dim: Symbol, tag: Symbol, body: T) extends CCExpression

case class ShareExpr[S <: ASTNode, T <: ASTNode](name: Symbol, exp: S, body: T) extends CCExpression

// TODO In both cases the body-parser needs to be stored
case class IdExpr[T <: ASTNode, P](name: Symbol, context: P) extends CCExpression
case class IncludeExpr[T <: ASTNode, P](filename: String, context: P) extends CCExpression

/** Nodes needed for implementation purpose only */
case class PartialConfig(body: Expression, selections: List[(Symbol, Symbol)]) extends CCExpression