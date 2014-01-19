package choicecalculus

import phases.{ Parser, Reader, Namer, DimensionChecker, Evaluator }

import utility.messages._

import lang.trees.Tree
import scala.collection.immutable.Seq
import org.kiama.util.FileEmitter

trait Compiler extends Pipeline[CompilerConfig] with Configurable[CompilerConfig] {

  def main(args: Array[String]): Unit = try {
    driver(args.toIndexedSeq)
  } catch {
    case err: FatalPhaseError => err.report
  } finally {
    reportFiltered
  }

  /**
   * Processes the arguments, creates configs and triggers `processfiles`
   */
  def driver(args: Seq[String]) {
    conf = new CompilerConfig(args)

    (conf.input(), conf.output()) match {
      case (in, Nil) => processfiles(in)

      case (in, out) if in.size != out.size => 
        raise("Please provide equal numbers for inputs and outputs")

      case (in, out) => out.zip(processfiles(in)).foreach {
        case (out, result) => {
          val file = new FileEmitter(out)
          file.emit(result)
          file.close()
        }
      }
    }
  }

  def processfiles(filenames: Seq[String]): Seq[String] = 
    for (filename <- filenames)
      yield processfile(filename)
}