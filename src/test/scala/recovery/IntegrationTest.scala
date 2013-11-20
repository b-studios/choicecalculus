package choicecalculus
package recovery

import org.scalatest._
import org.scalatest.matchers.ShouldMatchers._

import utility.Table
import lang.javascript.AtomLit

class IntegrationTest extends FlatSpec {

  def performTableTest(table: CloneInstanceTable) {
  
    object recovery extends CCRecovery {
      val result = process(table)
      tableFromChoices(result) should equal (table)
    }
    
  }

  def createRows(l: Seq[List[Int]]): Set[List[AtomLit]] = 
    l.toSet.map( (s: List[Int]) => s.map(int2atom))

  implicit def int2atom(n: Int): AtomLit = AtomLit(n.toString)

  it should "generate variational equivalent expressions for tables" in {
    
    // A<1,2> + A<1, B<1,2>>
    performTableTest(new CloneInstanceTable('x, 'y) {
      | (1) | (1) |;
      | (2) | (1) |;
      | (2) | (2) |;
    })
    
    // A<B<1,2>,3> + B<4,5> + C<1, B<2,3>>
    performTableTest(new CloneInstanceTable('x, 'y, 'z) {
      | (1) | (4) | (1) |;
      | (1) | (4) | (2) |;
      | (2) | (5) | (1) |;
      | (2) | (5) | (3) |;
      | (3) | (4) | (1) |;
      | (3) | (4) | (2) |;
      | (3) | (5) | (1) |;
      | (3) | (5) | (3) |;
    })
    
    performTableTest(new CloneInstanceTable('x, 'y, 'z) {
      | (1) | (4) | (1) |;
      | (2) | (4) | (2) |;
      | (2) | (5) | (2) |;
    })    
  }
  
  it should "generate variational equivalent expressions for all binary dimensions" in {
    
    val rows = createRows(for {
      x <- 1 to 2
      y <- 1 to 2
      z <- 1 to 2
      a <- 1 to 2
    } yield x :: y :: z :: a :: Nil)
    
    rows.subsets.foreach { rows => 
      val table = new CloneInstanceTable('x, 'y, 'z, 'a)
      performTableTest(table.addRows(rows.toList))
    }
  }
  
  it should "generate variational equivalent expressions including 5-ary dimensions" in {
    
    val rows = createRows(for {
      x <- 1 to 2
      y <- 1 to 2
      z <- 1 to 5
    } yield x :: y :: z :: Nil)
    
    rows.subsets.foreach { rows => 
      val table = new CloneInstanceTable('x, 'y, 'z)
      performTableTest(table.addRows(rows.toList))
    }
  }
}