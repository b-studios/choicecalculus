package choicecalculus
package phases

import org.scalatest._

import lang.jscc.JsCcParser

import utility.test
import utility.messages._

class ReaderTest extends FlatSpec with matchers.ShouldMatchers {

  import lang.trees.{ Dimension, Select, Include }
  import lang.javascript.trees.{ BlockStmt, Program, VarDeclStmt, VarBinding, EmptyStmt }

  val pathPrefix = "src/test/data/"

  trait Context extends Reader with JsCcParser with test.Helpers {

    val test01tree = dim ('A)('a, 'b) {
      BlockStmt(List(VarDeclStmt(List(
        VarBinding(lit("foo"), lit("42"))
      ))))
    }

    val test02tree = dim ('B)('a, 'b) {
      BlockStmt(List(VarDeclStmt(List(
        VarBinding(lit("bar"), lit("43"))
      ))))
    }

    def reading(filename: String): (SourceFile, List[SourceFile]) =
      runReader(pathPrefix + filename)
  }

  it should "read simple js.cc files without further ado" in new Context {

    val src1 = reading("reader_test01.js.cc")
    val src2 = reading("reader_test02.js.cc")

    (src1: @unchecked) match {
      case (src, Nil) => src.trees should equal (Seq(Program(List(test01tree))))
    }

    (src2: @unchecked) match {
      case (src, Nil) => src.trees should equal (Seq(Program(List(test02tree))))
    }
  }

  it should "always return the same source when reading files multiple times" in new Context {

    val (src1, Nil) = reading("reader_test01.js.cc")
    val (src2, Nil) = reading("reader_test01.js.cc")

    (src1 eq src2) should be (true)
  }

  it should "include simple dependencies" in new Context {

    val (src3, deps) = reading("reader_test03.js.cc")

    val (src1, Nil) = reading("reader_test01.js.cc")
    val (src2, Nil) = reading("reader_test02.js.cc")

    (src3.trees.head: @unchecked) match {
      case Program(List(Dimension(_, _, BlockStmt(List(
          Select(_, _, inc1: Include), EmptyStmt,
          Select(_, _, inc2: Include), EmptyStmt
        ))))) => {

        inc1->tree should equal (test01tree)
        inc2->tree should equal (test02tree)
      }
    }

    deps should equal (List(src1, src2))
  }

  it should "include dependencies recursively" in new Context {

    val (src4, deps) = reading("reader_test04.js.cc")

    val (src1, Nil) = reading("reader_test01.js.cc")
    val (src2, Nil) = reading("reader_test02.js.cc")
    val (src3, Nil) = reading("reader_test03.js.cc")

    // Find the tree that starts with `Dimension` (instead of `Program`)
    val test03tree = src3.trees.collectFirst {
      case d: Dimension => d
    }.get

    (src4.trees.head: @unchecked) match {
      case Program(List(Dimension(_, _, BlockStmt(List(
          Select(_, _, inc1: Include), EmptyStmt,
          Select(_, _, inc3: Include), EmptyStmt
        ))))) => {

        inc1->tree should equal (test01tree)
        inc3->tree should equal (test03tree)
      }
    }

    deps should equal (List(src1, src2, src3))
  }

  it should "throw a FatalPhaseError if file cannot be read" in new Context {

    evaluating {
      reading("reader_testXX.js.cc")
    } should produce [FatalPhaseError]

  }

  it should "throw a FatalPhaseError if cyclic dependencies have been detected" in new Context {

    evaluating {
      reading("reader_test05.js.cc")
    } should produce [FatalPhaseError]

    evaluating {
      reading("reader_test06.js.cc")
    } should produce [FatalPhaseError]

  }
}