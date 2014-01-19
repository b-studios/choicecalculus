package choicecalculus.lang
package clonedetection
package trees

import javascript.trees.{ Expression, Statement, Literal }

abstract class CloneDetectionNode extends Expression with Statement with Literal
case class CloneVar(name: Symbol) extends CloneDetectionNode