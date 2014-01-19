package choicecalculus
package phases

import org.scalatest._

import lang.trees.{ Identifier, Tree }
import lang.jscc.JsCcParser

import org.kiama.attribution.Attribution.initTree

import utility.test
import utility.messages._

class NamerTest extends FlatSpec with matchers.ShouldMatchers {

  trait Context extends Namer with test.Helpers with namer.SymbolPreservingRewriter {

    // equal is already defined in rewriter ...
    import org.scalatest.matchers.ShouldMatchers.{ equal => equal_ }

    def naming(tree: Tree): Tree = {
      initTree(tree);
      runNamer(tree);
    }

    def unboundError[T](block: => T): FatalPhaseError = {
      evaluating { block } should produce [FatalPhaseError] match {
        case err => {
          err.phase should equal_('namer)
          err.message.contains("unbound") should be (true)
          err
        }
      }
    }
  }

  it should "bind identifier to the corresponding share expression" in new Context {
    val v = id('v)
    val bindingShare = share('v, lit("3") + lit("4"), id('v) + v)

    naming(bindingShare)
    (v->symbol).definition should be (bindingShare)
  }

  it should "bind identifiers with shadowed shares" in new Context {

    val v1 = id('v)
    val v2 = id('v)

    val bindingShare2 = share('v, lit("3") + lit("4"), v2)
    val bindingShare1 = share('v, lit("1"), v1 + bindingShare2)

    naming(bindingShare1)
    (v1->symbol).definition should be (bindingShare1)
    (v2->symbol).definition should be (bindingShare2)
  }

  it should "fail binding unbound identifiers" in new Context {

    val v = id('v)
    val w = id('w)

    unboundError { naming(dim('A)('a,'b) { v + w }) }
  }

  it should "not create recursive bindings" in new Context {
    val v1 = id('v)
    val v2 = id('v)

    val bindingShare2 = share('v, v1, v2 + lit("42"))
    val bindingShare1 = share('v, lit("3") + lit("4"), bindingShare2)

    naming(bindingShare1)
    (v1->symbol).definition should be (bindingShare1)
    (v2->symbol).definition should be (bindingShare2)
  }
}