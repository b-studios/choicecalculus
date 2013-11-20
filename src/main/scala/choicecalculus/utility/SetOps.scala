package choicecalculus
package utility

object setops {

  implicit class SetOps[T](self: Set[T]) {
      
    // http://stackoverflow.com/questions/11581175/how-to-generate-the-power-set-of-a-set-in-scala
    def power: Set[Set[T]] = {
      def pwr(t: Set[T], ps: Set[Set[T]]): Set[Set[T]] =
        if (t.isEmpty) ps
        else pwr(t.tail, ps ++ (ps map (_ + t.head)))

      pwr(self, Set(Set.empty[T])) //Powerset of ∅ is {∅}
    }
    
    // {1, 2, 3} -> { {{1,2,3}, {}}, {{1}, {2,3}}, {{1,2}, {3}} }
    // O(2^(i-1))
    def allSplittings: Set[Set[Set[T]]] = 
      for {
        subset <- power
      } yield Set(subset, self -- subset)
      
  }
  
}
