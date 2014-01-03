package choicecalculus
package utility

import combinatorics._

/**
 6:    149ms
 7:    334ms
 8:    513ms
 9:    783ms
10:  1,353ms
11:  3,648ms
12: 13,804ms
13:        ? (Out of heap space)

so we probably need good euristics for n > 12
*/
abstract class ColoringBruteforceSolver[T] {
  
  type Solution = Set[Set[T]]

  def connected(n1: T, n2: T): Boolean
  
  def solve(input: Set[T]): Solution = 
    solveRec(Set(Set(input)))
  
  // sets: {{{1,2,3}}, ...}  
  private def solveRec(sets: Set[Solution]): Solution = {

    val sols = sets.filter { isSolution(_) }
    
    if (sols.size >= 1)
      return sols.toList.sortBy { _.size }.head
      
    solveRec( sets.flatMap { 
      case s if isSolution(s) => Set(s)
      case s => 
        s.foldLeft[Set[Solution]](Set(Set.empty)) {
        case (acc, group) if disjoint(group) => for {
            a <- acc
          } yield a + group
                
        case (acc, group) => for {
            a <- acc
            c <- group.allSplittings -- Set(Set(group, Set.empty), Set.empty)
          } yield a ++ c
      }
    })
  }
    
  private def disjoint(group: Set[T]): Boolean = 
    (for {
      n1 <- group;
      n2 <- group;
      if connected(n1, n2)
    } yield false).isEmpty
  
  // check whether a given solution is a solution
  private def isSolution(sol: Solution): Boolean = 
    sol.forall { disjoint(_) }
}