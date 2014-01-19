package choicecalculus
package utility

import scala.collection.mutable._
import scala.collection.generic._
import scala.collection.convert.Wrappers._

class IdentityHashMap[A, B] extends JMapWrapper[A, B](new java.util.IdentityHashMap)
    with JMapWrapperLike[A, B, IdentityHashMap[A, B]] {
  override def empty = new IdentityHashMap[A, B]
}

object IdentityHashMap extends MutableMapFactory[IdentityHashMap] {
  implicit def canBuildFrom[A, B]: CanBuildFrom[Coll, (A, B), IdentityHashMap[A, B]] = 
    new MapCanBuildFrom[A, B]

  def empty[A, B]: IdentityHashMap[A, B] = new IdentityHashMap[A, B]
}