package choicecalculus
package utility

package object combinatorics {

  /**
   * Iterates over all permutations of distributing the elements of `list`
   * over a new list of `size`. 
   *
   *
   * The given `size` has to be >= `list.size`. All free slots not filled with
   * elements of `list` are filled with `None`.
   * 
   * The first element of the input list is fixed to be also the first element 
   * of resulting list (of the Iterator). This is necessary to be able to exploit
   * symmetry.
   *
   * @param list the list to draw elements from
   * @param size the size of the new list
   *
   * @example {{{ permutations(List(1), 3) = Iterator(List(Some(1), None, None))  }}}
   * @example {{{
   *   permutations(List(1, 2), 3) = Iterator(List(Some(1), Some(2), None), 
   *     List(Some(1), None, Some(2)))
   * }}}
   */
  def permutations[T](list: List[T], size: Int): Iterator[List[Option[T]]] = {
    
    require(size >= list.size, s"Size has to be at least `list.size` ($list.size)")

    list match { 
      case head :: tail => 
        // Build up list with Options
        (tail.map(Some(_)) ++ (list.size until size).map(_ => None))
        .permutations
        // reattach head
        .map { perm => Some(head) +: perm }

      case empty => Iterator.empty
    }
  }

  /**
   * Adds methods for combinatorics to a given set
   *
   * Just import `utility.combinatorics._` and the methods are available
   * on sets.
   */
  implicit class SetOps[T](self: Set[T]) {
    
    /**
     * Computes the powerset of the given set
     *
     * @example {{{ 
     *   scala> Set(1,2).power
     *   res0: Set[Set[Int]] = Set(Set(), Set(1), Set(2), Set(1,2))
     * }}}
     *
     * @see http://stackoverflow.com/questions/11581175/how-to-generate-the-power-set-of-a-set-in-scala
     */
    def power: Set[Set[T]] = {
      def pwr(t: Set[T], ps: Set[Set[T]]): Set[Set[T]] =
        if (t.isEmpty) ps
        else pwr(t.tail, ps ++ (ps map (_ + t.head)))

      pwr(self, Set(Set.empty[T])) //Powerset of ∅ is {∅}
    }
    
    /**
     * Computes all possibilities to split a set into two parts.
     *
     * It returns Set[Set[Set[T]]] instead of Set[(Set[], Set[])] to 
     * eliminate ordering of the tuples.
     * 
     * @example {{{
     *   scala> Set(1,2,3).allSplittings
     *   res0: Set[Set[Set[Int]]]: Set(
     *     Set(Set(1,2,3), Set()), 
     *     Set(Set(1),     Set(2,3)), 
     *     Set(Set(1,2),   Set(3)))
     * }}}
     */
    def allSplittings: Set[Set[Set[T]]] = 
      for {
        subset <- power
      } yield Set(subset, self -- subset)
  }
}