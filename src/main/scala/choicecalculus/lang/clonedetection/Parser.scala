package choicecalculus.lang
package clonedetection

import javascript.JavaScriptParser

trait CloneDetectionParser extends JavaScriptParser {

  lazy val parser: PackratParser[ASTNode] = topLevel

  lazy val clone_id: PackratParser[CloneDetectionNode] =
    "[[#variable" ~> """[a-f0-9]+""".r <~ "]]" ^^ { n =>
      CloneVar(Symbol(n))
    }

  override def _literal = clone_id | super._literal
  override def _primExpr = clone_id | super._primExpr
  override def _assignExpr = clone_id | super._assignExpr
  override def _expression = clone_id | super._expression
  override def _statement = clone_id | super._statement
  override def _idLiteral = clone_id | super._idLiteral
}