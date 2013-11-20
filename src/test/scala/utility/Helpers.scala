package choicecalculus
package utility.test

import lang.ASTNode
import lang.choicecalculus.{ Choice, Choices, Dimension, Include, PartialConfig, Select, Share, SharedId }
import lang.javascript.{ AtomLit, BinaryOpExpr, Expression }

trait Helpers {
  
  /**
   * Constructor helpers
   */
  def dim[T <: ASTNode](name: Symbol, tags: List[Symbol], body: T) = Dimension(name, tags, body)
  def dim[T <: ASTNode](name: Symbol)(tags: Symbol*)(body: T) = Dimension(name, tags.toList, body)
  def choices[T <: ASTNode](dim: Symbol, choices: List[Choice[T]]) = Choices[T](dim, choices)
  def choices[T <: ASTNode](dim: Symbol)(choices: (Symbol, T)*) = Choices[T](dim, choices.map {
    case (tag, body) => Choice(tag, body)
  }.toList)
  def choice[T <: ASTNode](tag: Symbol, body: T) = Choice(tag, body)
  def select[T <: ASTNode](dim: Symbol, tag: Symbol, body: T) = Select(dim, tag, body)
  //def select[T <: ASTNode](dim: Symbol, tag: Symbol)(body: T) = Select(dim, tag, body)
  def share[S <: ASTNode, T <: ASTNode](name: Symbol, exp: S, body: T) = Share(name, exp, body)
  def id[T <: ASTNode](name: Symbol) = SharedId(name)
  def include[T <: ASTNode, P](filename: String, context: P) = Include(filename, context)
  def partialConfig[T <: ASTNode](selects: (Symbol, Symbol)*)(body: T) = PartialConfig(body, selects.toList)
  
  
  def lit(name: String) = AtomLit(name)
  
  
  case class BinOpConstructor(lhs: Expression) {
    def +(rhs: Expression) = BinaryOpExpr(lhs, "+", rhs)
    def *(rhs: Expression) = BinaryOpExpr(lhs, "*", rhs)
  }
  
  implicit def exp2binOp(e: Expression): BinOpConstructor = BinOpConstructor(e)
}
