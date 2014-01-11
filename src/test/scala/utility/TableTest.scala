package choicecalculus
package utility

import org.scalatest._
import org.scalatest.matchers.ShouldMatchers._

class TableTest extends FlatSpec {

  object table1 extends Table[Symbol, Int]('x, 'y, 'z) {
    | (1) | (2) | (3) |;
    | (4) | (5) | (6) |;
    | (7) | (8) | (9) |;
  }

  object table2 extends Table[Symbol, Int]('x, 'z, 'y) {
    | (4) | (6) | (5) |;
    | (1) | (3) | (2) |;
    | (7) | (9) | (8) |;
  }

  object table3 extends Table[Symbol, Int]('x, 'z, 'y) {
    | (4) | (6) | (5) |;
    | (1) | (3) | (2) |;
    | (7) | (9) | (8) |;
    | (5) | (3) | (2) |;
  }

  object table4 extends Table[Symbol, Int]('x, 'y, 'z) {
    | (1) | (2) | (3) |;
    | (4) | (5) | (6) |;
    | (7) | (8) | (9) |;
    | (4) | (5) | (6) |;
    | (7) | (8) | (9) |;
  }

  it should "create the table correctly" in {
    table1.rows should contain (List(1,2,3))
    table1.rows should contain (List(4,5,6))
    table1.rows should contain (List(7,8,9))
  }

  it should "recognize equal tables as equal" in {
    (table1 == table2) should be (true)
    (table2 == table1) should be (true)
    (table1 == table3) should be (false)
    (table3 == table1) should be (false)
    (table3 == table2) should be (false)
    (table2 == table3) should be (false)
    (table3 == table3) should be (true)
  }

  it should "eliminate duplicate rows" in {
    (table4 == table1) should be (true)
    (table1 == table4) should be (true)
  }

  it should "implement subset in terms of rows" in {
    table1.subsetOf(table3) should be (true)
  }

}