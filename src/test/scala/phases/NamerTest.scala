package choicecalculus
package phases

import org.scalatest._

import lang.ASTNode
import lang.choicecalculus.Identifier
import lang.JsCcParser

import org.kiama.attribution.Attribution.initTree

import utility.test
import utility.messages._

class NamerTest extends FlatSpec with matchers.ShouldMatchers {

  object namer extends Namer with test.Helpers {

    def naming(tree: ASTNode): ASTNode = {
      initTree(tree);
      runNamer(tree);
    }

    def unboundError[T](block: => T): FatalPhaseError = {
      evaluating { block } should produce [FatalPhaseError] match {
        case err => {
          err.phase should equal ('namer)
          err.message.contains("unbound") should be (true)
          err
        }
      }
    }

    it should "bind identifier to the corresponding share expression" in {
      val v = id('v)
      val bindingShare = share('v, lit("3") + lit("4"), id('v) + v)

      naming(bindingShare)
      v->bindingInstance should be (Some(bindingShare))
    }

    it should "bind identifiers with shadowed shares" in {

      val v1 = id('v)
      val v2 = id('v)

      val bindingShare2 = share('v, lit("3") + lit("4"), v2)
      val bindingShare1 = share('v, lit("1"), v1 + bindingShare2)

      naming(bindingShare1)
      v1->bindingInstance should be (Some(bindingShare1))
      v2->bindingInstance should be (Some(bindingShare2))
    }

    it should "fail binding unbound identifiers" in {

      val v = id('v)
      val w = id('w)

      unboundError { naming(dim('A)('a,'b) { v + w }) }
    }

    it should "not create recursive bindings" in {
      val v1 = id('v)
      val v2 = id('v)

      val bindingShare2 = share('v, v1, v2 + lit("42"))
      val bindingShare1 = share('v, lit("3") + lit("4"), bindingShare2)

      naming(bindingShare1)
      v1->bindingInstance should be (Some(bindingShare1))
      v2->bindingInstance should be (Some(bindingShare2))
    }

  }

  namer
}