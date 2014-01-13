package choicecalculus
package phases
package namer

trait SymbolPreservingRewriter extends org.kiama.rewriting.CallbackRewriter { self: Namer => 

  import lang.choicecalculus.{ Choice, Dimension, Identifier, Share }

  def rewriting[T](oldTerm: T, newTerm: T): T = {
    (oldTerm, newTerm) match {

      // Binding instances
      case (oldDim: Dimension[_], newDim: Dimension[_]) =>
        oldDim->moveSymbolTo(newDim)

      case (oldShare: Share[_,_], newShare: Share[_,_]) =>
        oldShare->moveSymbolTo(newShare)

      // Bound intances
      case (oldId: Identifier[_], newId: Identifier[_]) =>
        oldId->copySymbol(newId)

      case (oldChoice: Choice[_], newChoice: Choice[_]) =>
        oldChoice->copySymbol(newChoice)

      case _ =>
    }
    newTerm
  }
}