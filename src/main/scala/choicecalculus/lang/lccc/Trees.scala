package choicecalculus
package lang.lccc
package trees

import lang.trees.Tree

case class Lambda(arg: Tree, body: Tree) extends Tree
case class App(fun: Tree, arg: Tree) extends Tree
case class BinaryOp(lhs: Tree, op: String, rhs: Tree) extends Tree
case class Literal(text: String) extends Tree
case class Grouping(tree: Tree) extends Tree