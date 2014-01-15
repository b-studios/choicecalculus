package choicecalculus

import utility.messages._

import lang.trees.Tree

trait Repl extends org.kiama.util.REPL with Pipeline {

  def processline(line: String): Unit = try {
    resetMessages()
    processfile(createVirtualFile(line))
  } catch {
    case err: FatalPhaseError => err.report
  } finally {
    report
  }

}