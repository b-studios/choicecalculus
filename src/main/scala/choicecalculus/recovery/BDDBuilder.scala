package choicecalculus
package recovery

import scala.math.{ ceil, floor, log }
import utility.memoization._

/**
 * A Binary Decision Diagram Builder
 *
 * The constructor takes two arguments that are necessary to build a tree. They 
 * are chosen to be used convenient with case classes.
 *
 * It is recommended to instantiate <strong>only one builder</strong> per tree 
 * since otherwise nodes are shared between multiple trees.
 *
 * @tparam Value the type of the leafs' values.
 *
 * @tparam Node the (probably) abstract type of the tree nodes. Has to be a
 *              reference type, since `eq` is used to compare the nodes.
 *
 * @tparam Leaf the type of the tree's leafs. Has to be a node.
 *
 * @constructor Creates a BDDBuilder using the provided builder functions for
 *              leafs and nodes.
 *
 * @param buildLeaf a function that should build a new instance of `Leaf`
 *                  when provided with a `Value`.
 *
 * @param buildNode a function that should build a new instance of `Node`
 *                  when provided with two child nodes.
 *
 * @example {{{
 *   // Some sample classes to build up trees of `Int`
 *   trait Tree
 *   case class BinaryNode(lhs: Tree, rhs: Tree) extends Tree
 *   case class Leaf(value: Int) extends Tree
 *   
 *   // Note that both occurrences of `Leaf(1)` in the
 *   // following result refer to the exact same instance:
 *   BDDBuilder(Leaf, BinaryNode) build (List(1,1,0,1))
 *   // => BinaryNode(Leaf(1), BinaryNode(Leaf(0), Leaf(1)))
 * }}}
 */
class BDDBuilder[Value, Node <: AnyRef, Leaf <: Node](
  buildLeaf: Value => Leaf,
  buildNode: (Node, Node) => Node
) {

  /**
   * Bottom up constructs a reduced tree corresponding to the 
   * full binary tree for `values`.
   *
   * @param values has to contain at least one element and
   *        it's size needs to be a power of 2
   *
   * @return a reduced tree corresponding to the full binary tree
   *
   * @example {{{ 
   *   val bdd: BinaryDecisionDiagram = ...
   *
   *   // Note that both occurrences of `Leaf(1)` in the
   *   // following result refer to the exact same instance:
   *   bdd.build(List(1,1,0,1))
   *   // => Node(Leaf(1), Node(Leaf(0), Leaf(1)))
   * }}}
   */
  def build(values: List[Value]): Node = {

    val size = values.size

    (log(size) / log(2)) match { case log2 =>
      require(size >= 1, "`leafs` has to contain at least one element")
      require(ceil(log2) == floor(log2), "`leafs` has to be of size (2^x)")
    }

    values match {
      case head :: Nil => hashedLeaf(head)
      case _ => values.splitAt(size/2) match {
        case (firstHalf, secondHalf) => 
          hashedNode (build(firstHalf)) (build(secondHalf))
      }
    }
  }

  private lazy val hashedLeaf: Value => Leaf = memoized { 
    case v => buildLeaf(v)
  }

  /**
   * In addition to hash consing performs simple reduction
   * if both childnodes are the same.
   *
   * Is curried in order to allow memoization
   */
  private lazy val hashedNode: Node => Node => Node = memoized {
    case l => memoized {
      case r if l eq r => l
      case r => buildNode(l, r)
    }
  }
}
object BDDBuilder {

  /**
   * Returns a [[BDDBuilder]] for full binary trees
   *
   * @see [[BDDBuilder]]
   */
  def apply[Value, Node <: AnyRef, Leaf <: Node](
    buildLeaf: Value => Leaf,
    buildNode: (Node, Node) => Node
  ) = new BDDBuilder(buildLeaf, buildNode)

  /**
   * Returns a [[BDDBuilder]] for full binary trees with optional leafs
   *
   * Taking the same two builder functions as arguments as the constructor 
   * of [[BDDBuilder]] this method returns a BDDBuilder optimized for trees 
   * with optional leafs.
   * 
   * The tree, as built by the builder, will be of type `Option[Node]`
   *
   * @example {{{
   *   trait Tree
   *   case class BinaryNode(lhs: Tree, rhs: Tree) extends Tree
   *   case class Leaf(value: Int) extends Tree
   *   
   *   // for every use of build, create a new instance of the builder   
   *   def builder = BDDBuilder.option(Leaf, BinaryNode)
   *
   *   builder build (List(Some(1), Some(1), None, Some(0)))
   *   // => Some(BinaryNode(Leaf(1), Leaf(0)))
   *
   *   builder build (List(Some(1), None, Some(0), Some(1)))
   *   // => Some(BinaryNode(Leaf(1), BinaryNode(Leaf(0), Leaf(1))))
   *   
   *   builder build (List(None, None, None, None))
   *   // => None
   * }}}
   *
   * @see [[BDDBuilder]]
   */
  def option[Value, Node <: AnyRef, Leaf <: Node](
    buildLeaf: Value => Leaf,
    buildNode: (Node, Node) => Node
  ) = new BDDBuilder(
    (v: Option[Value]) => v map buildLeaf,
    (l: Option[Node], r: Option[Node]) => (l, r) match {
      case (Some(lhs), Some(rhs)) => Some(buildNode(lhs,rhs))
      case (lhs, rhs) => lhs orElse rhs        
    }
  )
}