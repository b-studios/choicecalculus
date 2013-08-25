package choicecalculus.lang
package choicecalculus

import javascript.{ Expression, Statement, Literal }

abstract class ChoiceCalculusNode extends Expression with Statement with Literal
case class Dimension[T <: ASTNode](name: Symbol, tags: List[Symbol], body: T) extends ChoiceCalculusNode

case class Choices[T <: ASTNode](dim: Symbol, choices: List[Choice[T]]) extends ChoiceCalculusNode

// a single choice is a mapping from tag to host language expression - can only occur as immediate
// child of choice-expressions
case class Choice[T <: ASTNode](tag: Symbol, body: T) extends ASTNode

case class Select[T <: ASTNode](dim: Symbol, tag: Symbol, body: T) extends ChoiceCalculusNode

case class Share[S <: ASTNode, T <: ASTNode](name: Symbol, exp: S, body: T) extends ChoiceCalculusNode
case class SharedId[T <: ASTNode](name: Symbol) extends ChoiceCalculusNode

case class Include[T <: ASTNode, P](filename: String, context: P) extends ChoiceCalculusNode

case class PartialConfig[T <: ASTNode](body: T, selections: List[(Symbol, Symbol)]) extends ChoiceCalculusNode