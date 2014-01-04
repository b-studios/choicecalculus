package choicecalculus
package recovery
package graphbased

import org.scalatest._
import org.scalatest.matchers.ShouldMatchers._

import utility.Table

class ProblemGraphTest extends FlatSpec {

  def build(table: Table[Symbol, Int]): ProblemGraph = {
    object builder extends ProblemGraphBuilder[Int]
    builder.createFromTable(table)
  }


  object table1 extends Table[Symbol, Int]('x, 'y) {
    | (1) | (1) |;
    | (2) | (1) |;
    | (2) | (2) |;
  }

  val table2 = new Table[Symbol, Int]('w, 'x, 'y) {
      | (1) | (1) | (1) |;
      | (2) | (1) | (1) |;
      | (2) | (2) | (2) |;
      | (2) | (3) | (3) |;
    }
 
  it should "create the problem graph with the correct structure" in {

    val ProblemGraph(vs, es) = build(table1)

    vs.size should be (2)
    vs.values.foreach { _.size should be (2) }
    es.size should be (3)
  }

  it should "create correct graph for minimality counter example" in {
      
    val ProblemGraph(vs, es) = build(table2)

    vs.size should be (3)
    vs.map { case (_, n) => n}
      .groupBy(_.size)
      .map { case (k, v) =>  (k, v.size) } should be (Map(2 -> 1, 3 -> 2))
    es.size should be (20)
  }
  
}
