package choicecalculus
package recovery

import org.scalatest._
import org.scalatest.matchers.ShouldMatchers._

import utility.Table

class ProblemGraphTest extends FlatSpec {

  object table extends Table[Symbol, Int]('x, 'y) {
    | (1) | (1) |;
    | (2) | (1) |;
    | (2) | (2) |;
  }
  
  object builder extends ProblemGraphBuilder[Int]
    
  val ProblemGraph(vs, es) = builder.createFromTable(table)
 
  it should "create the problem graph with the correct structure" in {
    vs.size should be (2)
    vs.values.foreach { _.size should be (2) }
    es.size should be (3)
  }
  
}
