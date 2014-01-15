package choicecalculus

import utility.Table
import lang.trees.Tree

package object recovery {

  private[recovery] type Dimension = (Symbol, Set[Symbol])

  private[recovery] type CloneInstanceTable = Table[
    Symbol,  // variable name
    Tree     // value / instance
  ]

}