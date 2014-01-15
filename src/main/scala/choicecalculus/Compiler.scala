package choicecalculus

import phases.{ Parser, Reader, Namer, DimensionChecker, Evaluator }

import utility.messages._

import lang.trees.Tree

trait Compiler extends Pipeline {

  def main(args: Array[String]) {
    driver(args.toIndexedSeq)
  }

  /**
   * The driver for this compiler. Currently no config is used
   * so the arguments are directly passed as filenames
   */
  def driver(args: Seq[String]) {
    processfiles(args)
  }

  def processfiles(filenames: Seq[String]): Unit = try {
    for (filename <- filenames) {
      processfile(filename)
    }
  } catch {
    case err: FatalPhaseError => err.report
  } finally {
    report
  }

}