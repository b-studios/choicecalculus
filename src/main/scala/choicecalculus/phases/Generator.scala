package choicecalculus
package phases

import lang.trees.Tree

/**
 * <h2>The Generator phase
 *
 * This is just the interface for a generator. It can be implemented by
 * a code generator or a debug generator.
 */
trait Generator {

  def runGenerator(source: Tree): String

}