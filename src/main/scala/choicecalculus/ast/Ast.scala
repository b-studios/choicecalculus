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
case class DimensionExpr(name: Symbol, tags: List[Symbol], body: ASTNode) extends CCExpression

case class ChoiceExpr(dim: Symbol, choices: List[Choice]) extends CCExpression

// a single choice is a mapping from tag to host language expression - can only occur as immediate
// child of choice-expressions
case class Choice(tag: Symbol, body: ASTNode) extends ASTNode

case class SelectExpr(dim: Symbol, tag: Symbol, body: ASTNode) extends CCExpression

case class ShareExpr(name: Symbol, exp: ASTNode, body: ASTNode) extends CCExpression

case class IdExpr(name: Symbol) extends CCExpression

case class IncludeExpr(filename: String) extends CCExpression


/** Nodes needed for implementation purpose only */
case class PartialConfig(body: Expression, selections: List[(Symbol, Symbol)]) extends CCExpression