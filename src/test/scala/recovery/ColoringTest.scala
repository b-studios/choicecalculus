package choicecalculus
package recovery

import org.scalatest._
import org.scalatest.matchers.ShouldMatchers._

import utility.ColoringBruteforceSolver

class ColoringTest extends FlatSpec {    
    
  object coloring extends ColoringBruteforceSolver[Int] {

    def connected(n1: Int, n2: Int): Boolean = (n1, n2) match {
      case (1, 2) => true
      case (2, 1) => true
      
      case (1, 3) => true
      case (3, 1) => true
      
      
      case (1, 4) => true
      case (4, 1) => true
      
      case (2, 4) => true
      case (4, 2) => true
      
      case (3, 2) => true
      case (2, 3) => true
      
      case (3, 4) => true
      case (4, 3) => true
      
      case (_, _) => false
    }
    
    it should "find the minimal solution" in {
      val input = Set(1,2,3,4,5)
      val solution = solve(input)
      solution.size should equal (4)
      solution.flatten should equal (input)
    }
  }
  coloring 
}