package choicecalculus
package recovery
package graphbased

import org.scalatest._
import org.scalatest.matchers.ShouldMatchers._

import labeling.{ Path, Label }

class PathCompressionTest extends FlatSpec {
  
  object recover extends CCRecovery {
    dimensions = Map(('A, 'a :: 'b :: Nil), ('B, 'a :: 'b :: 'c :: Nil))
    
    it should "remove fully expanded dimensions" in {
      val label = Label(Set(
        Path(('A,'a) :: ('B,'a) :: Nil),
        Path(('A,'b) :: ('B,'a) :: Nil)  
      ))
      allVariantsStripping(label)
      label should equal (Label(Set(Path(('B,'a) :: Nil))))
    }
    
    it should "not remove any dependend dimensions, that are not fully expanded" in {
      val label = Label(Set(
        Path(('A,'a) :: ('B,'a) :: Nil),
        Path(('A,'a) :: ('B,'b) :: Nil)  
      ))
      allVariantsStripping(label)
      label should equal (Label(Set(
        Path(('A,'a) :: ('B,'a) :: Nil),
        Path(('A,'a) :: ('B,'b) :: Nil)  
      )))
    }
    
    it should "remove fully expanded ternary dimensions" in {
      val label = Label(Set(
        Path(('A,'a) :: ('B,'a) :: Nil),
        Path(('A,'a) :: ('B,'b) :: Nil),
        Path(('A,'a) :: ('B,'c) :: Nil)
      ))
      allVariantsStripping(label)
      label should equal (Label(Set(Path(('A,'a) :: Nil))))
    }
    
    it should "remove all dimensions, if they are all fully expanded" in {
      val label = Label(Set(
        Path(('A,'a) :: ('B,'a) :: Nil),
        Path(('A,'a) :: ('B,'b) :: Nil),
        Path(('A,'a) :: ('B,'c) :: Nil),
        Path(('A,'b) :: ('B,'a) :: Nil),
        Path(('A,'b) :: ('B,'b) :: Nil),
        Path(('A,'b) :: ('B,'c) :: Nil)
      ))
      allVariantsStripping(label)
      label should equal (Label(Set(Path(Nil))))
    }
  }
  recover
}