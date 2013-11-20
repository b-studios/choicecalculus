package choicecalculus

import utility.Table
import lang.ASTNode

package object recovery {

  private[recovery] type CloneInstanceTable = Table[
    Symbol,  // variable name
    ASTNode  // value / instance
  ]

}