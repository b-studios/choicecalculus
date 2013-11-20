package choicecalculus
package recovery

import org.scalatest._
import org.scalatest.matchers.ShouldMatchers._

import recovery.CCRecovery

class PathCompressionTest extends FlatSpec {
  
  object recover extends CCRecovery[Nothing] {
    dimensions = Map(('A, 'a :: 'b :: Nil), ('B, 'a :: 'b :: 'c :: Nil))
    
    it should "remove fully expanded dimensions" in {
      val label = Label(Set(
        Path(('A,'a) :: ('B,'a) :: Nil),
        Path(('A,'b) :: ('B,'a) :: Nil)  
      ))
      label.allVariantsStripping
      label should equal (Label(Set(Path(('B,'a) :: Nil))))
    }
    
    it should "not remove any dependend dimensions, that are not fully expanded" in {
      val label = Label(Set(
        Path(('A,'a) :: ('B,'a) :: Nil),
        Path(('A,'a) :: ('B,'b) :: Nil)  
      ))
      label.allVariantsStripping
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
      label.allVariantsStripping
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
      label.allVariantsStripping
      label should equal (Label(Set(Path(Nil))))
    }
  }
  recover
}