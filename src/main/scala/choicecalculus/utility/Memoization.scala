package choicecalculus
package utility

/**
 * Package contains utility functions for memoization
 *
 * @see memoization#memoized[K,V](K => V)
 */
package object memoization {

  import scala.collection.mutable

  /**
   * Allows memoization of single argument functions
   *
   * Just reimplemented here to avoid dependencies to Scalaz.
   *
   * @example {{{
   *   lazy val factorial: Int => Int = memoized {
   *     case 0 => 1
   *     case n => factorial(n-1) * n
   * }}}
   */
  def memoized[K, V](impl: K => V): K => V = new Memoizer[K, V](impl)

  private class Memoizer[K, V](impl: K => V) extends Function1[K, V] {
    lazy val store = new mutable.HashMap[K, V]
    def apply(k: K): V = store getOrElseUpdate (k, impl(k))
  }
}