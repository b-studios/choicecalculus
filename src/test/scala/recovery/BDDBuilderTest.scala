package choicecalculus
package recovery

import utility.combinatorics._

import org.scalatest._
import org.scalatest.matchers.ShouldMatchers._

class BDDBuilderTest extends FlatSpec with utility.test.Helpers {

  type Value = Int

  sealed trait Node
  case class Choice(lvl: Int, l: Node, r: Node) extends Node
  case class Leaf(v: Value) extends Node
    
  locally {

    def builder = BDDBuilder(Leaf, Choice)

    def countLeafs(n: Node): Int = n match {
      case _:Leaf => 1
      case Choice(_,l,r) => countLeafs(l) + countLeafs(r)
    }

    def leafsFor(values: Value*): Int = 
      countLeafs(builder build(values.toList))

    it should "create the reduced OBDD graph for a given list of values" in {
      leafsFor(1,1,0,1) should equal (3)
      leafsFor(1,0,1,0) should equal (2)
      leafsFor(1,1,1,1) should equal (1)
      leafsFor(1,0,0,0) should equal (3)
      leafsFor(1,0,1,0,1,0,1,0) should equal (2)
      leafsFor(1,1,1,1,0,0,0,0) should equal (2)
    }
  }

  locally {

    def builder = BDDBuilder.option(Leaf, Choice)

    def countLeafs(n: Option[Node]): Int = n match {
      case None => 0
      case Some(Leaf(_)) => 1
      case Some(Choice(_,l,r)) => countLeafs(Some(l)) + countLeafs(Some(r))
    }

    it should "create the reduced OBDD graph for option values" in {

      for {
        list <- List(1).permutations(4)
        res = builder build list
      } countLeafs(res) should be (1)

      for {
        list <- List(1, 1).permutations(4)
        res = builder build list
      } countLeafs(res) should be (1)

      for {
        list <- List(1, 1, 1).permutations(4)
        res = builder build list
      } countLeafs(res) should be (1)

      for {
        list <- List(1, 1, 1, 1).permutations(4)
        res = builder build list
      } countLeafs(res) should be (1)

      for {
        list <- List(1, 2).permutations(4)
        res = builder build list
      } countLeafs(res) should be (2)

      (for {
        list <- List(1, 1, 2, 2).permutations(4)
        res = builder build list
      } yield countLeafs(res)).toList should equal (List(2, 2, 4)) 

      builder build (List(None, None, None, None)) should be (None)
    }
  }

}