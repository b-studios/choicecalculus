package choicecalculus
package recovery
package graphbased

import org.scalatest._
import org.scalatest.matchers.ShouldMatchers._

import utility.ColoringBruteforceSolver

class ColoringEncoding extends FlatSpec {
    
  /* x  y
   * 1  1
   * 1  2
   * 2  2
   *
   * connect all rows, connect all same values in one column
   * invert graph.
   */
  object coloring extends ColoringBruteforceSolver[Int] {

    def connected(n1: Int, n2: Int): Boolean = (n1, n2) match {
      case (1, 5) => true
      case (1, 3) => true
      case (1, 6) => true
      
      case (2, 3) => true
      case (2, 4) => true
      case (2, 6) => true
      
      case (3, 1) => true
      case (3, 2) => true
      case (3, 4) => true
      case (3, 5) => true
      
      case (4, 2) => true
      case (4, 3) => true
      case (4, 5) => true
      case (4, 6) => true
      
      case (5, 1) => true
      case (5, 3) => true
      case (5, 4) => true
      
      case (6, 1) => true
      case (6, 2) => true
      case (6, 4) => true
      
      case (_, _) => false
    }
    
    it should "find the minimal solution" in {
      val input = Set(1,2,3,4,5,6)
      val solution = solve(input)
      
      println(solution)
      //solution.size should equal (4)
      //solution.flatten should equal (input)
    }
  }
  coloring 
}