package choicecalculus
package ast

import org.kiama.attribution.Attributable
import org.kiama.util.Positioned
import utility.Rebuildable

import scala.util.parsing.combinator.Parsers

abstract class ASTNode extends Attributable with Positioned


/**
 * Expressions of the Choice Calculus (CC)
 * 
 * this is undesired, since there is a dependency to host language here, we need some constructor methods like:
 * 
 * createDim[T](name: Symbol, tags: List[Symbol], body: T) = new DimensionExpr[T](name, tags, body) with T
 * 
 * probably have to encode this using typeclasses like:
 * 
 * createDim[T](..., body: T)(implicit builder: DimBuilder[T]) = builder.build(name, tags, body)
 * 
 * trait DimBuilder[T] {
 *   def build(name: Symbol, tags: List[Symbol], body: T): "DimensionExpr with T"
 * }
 * 
 * implicit val dimExprBuilder: DimBuilder[Expression] = new DimBuilder {
 *   def build(name: Symbol, tags: List[Symbol, body: Expression): Expression = 
 *     new DimensionExpr[Expression](name, tags, body) with Expression
 * }
 * 
 */
abstract class CCExpression extends ASTNode
case class DimensionExpr[+T <: ASTNode](name: Symbol, tags: List[Symbol], body: T) extends CCExpression

case class ChoiceExpr[+T <: ASTNode](dim: Symbol, choices: List[Choice[T]]) extends CCExpression

// a single choice is a mapping from tag to host language expression - can only occur as immediate
// child of choice-expressions
case class Choice[+T <: ASTNode](tag: Symbol, body: T) extends ASTNode

case class SelectExpr[+T <: ASTNode](dim: Symbol, tag: Symbol, body: T) extends CCExpression

case class ShareExpr[+T <: ASTNode, S <: ASTNode](name: Symbol, exp: S, body: T) extends CCExpression

abstract class ContextPreserving[+T <: ASTNode] extends CCExpression {
  
  // val context: Parser[T]
  
}

case class IdExpr[+T <: ASTNode](name: Symbol) extends ContextPreserving[T]

// case class IncludeExpr[T](filename: String, contentParser: Parser[T]) extends CCExpression
// but then we need to wrap the AST nodes inside of a trait with `self: Parsers =>`
case class IncludeExpr[+T <: ASTNode](filename: String) extends ContextPreserving[T]

/** Nodes needed for implementation purpose only */
case class PartialConfig[+T <: ASTNode](body: T, selections: List[(Symbol, Symbol)]) extends CCExpression



/**
 * Builder Traits (TypeClass)
 */
trait Builder[T <: ASTNode] {
  
  def dimension(name: Symbol, tags: List[Symbol], body: T): T
  def choice(dim: Symbol, choices: List[Choice[T]]): T
  def select(dim: Symbol, tag: Symbol, body: T): T
  def share[S <: ASTNode](name: Symbol, exp: S, body: T): T
  def id(name: Symbol): T
  def include(filename: String): T
}