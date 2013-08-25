package choicecalculus.lang
package clonedetection

import javascript.{ Expression, Statement, Literal }

abstract class CloneDetectionNode extends Expression with Statement with Literal
case class CloneVar(name: Symbol) extends CloneDetectionNode