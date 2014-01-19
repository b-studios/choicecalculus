package choicecalculus

import utility.messages._

import lang.trees.Tree

import org.kiama.util.Emitter
import scala.collection.immutable.Seq
import org.kiama.util.REPLBase

trait Repl extends REPLBase with Pipeline[ReplConfig] with Configurable[ReplConfig] {

  def emitter = new Emitter

  def banner = """Welcome to the Choice Calculus REPL. Just enter your variational 
                 |programs and experience the magic!""".stripMargin

  override def setup(args: Array[String]) = {
    println(banner)
    conf = new ReplConfig(args.toIndexedSeq)
    true
  }

  def processline(line: String): Unit = try {
    resetMessages()
    println(processfile(createVirtualFile(line)))
  } catch {
    case err: FatalPhaseError => err.report
  } finally {
    reportFiltered
  }
}