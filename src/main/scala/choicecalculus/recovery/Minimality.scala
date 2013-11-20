package choicecalculus
package recovery

import utility.Table

private[recovery] trait Minimality[T] { self: Choices[T] with CCRecovery[T] =>
  
  type Solution = Map[Symbol, CCExpr]
  
  private def numberOfLeafs(sol: Solution): Int = 
    sol.map { case (_, c) => numberOfLeafs(c) }.sum
    
  private def numberOfLeafs(sol: CCExpr): Int = sol match {
    case CCChoice(dim, cases) => cases.map { 
      case CCCase(_, c) => numberOfLeafs(c)
    }.sum
    
    case _:Literal => 1
  } 
  
  private def numberOfDims(sol: Solution): Int = 
    sol.flatMap { 
      case (_, c) => collectDims(c) 
    }.toSet.size
    
  private def collectDims(sol: CCExpr): Set[Symbol] = sol match {
    case CCChoice(dim, cases) => Set(dim) ++ cases.flatMap { 
      case CCCase(_, c) => collectDims(c)
    }.toSet
    
    case _ => Set.empty
  }
 
  implicit object SolutionOrdering extends Ordering[Solution] {
    def compare(a: Solution, b: Solution) = 
      (numberOfLeafs(a) compare numberOfLeafs(b)) match {
        case 0 => numberOfDims(a) compare numberOfDims(b)
        case n => n      
      }
  }
  
  def isSolution(sol: Solution)(implicit table: Table[Symbol, T]): Boolean =
    tableFromChoices(sol) == table
  
  def minimalSolution(implicit table: Table[Symbol, T]): Solution = {
    generateAllSolutions.filter(isSolution).sorted.head
  }
  
  trait ChoiceShape
  case class ChoiceNode(dim: Symbol, choices: Map[Symbol, ChoiceShape]) extends ChoiceShape {
    override def toString: String = s"$dim<$choices>"
  }
  case object Hole extends ChoiceShape {
    override def toString: String = "☐"
  }
  
  // does not contain duplicates like a -> 1, b -> 1
  // example: 
  //     scala> combinatoricMapping(List(1,2,3), List('a,'b))
  //     res0: Set[Map[Symbol,Int]] = Set(Map('a -> 1, 'b -> 2), Map('a -> 1, 'b -> 3), Map('a -> 2, 'b -> 3))
  def combinatoricMappingNoDup[S,K](src: Iterable[S], keys: Iterable[K]): Set[Map[K,S]] =
    src.toList.combinations(keys.size).map(c => (keys zip c).toMap).toSet

  // This implementation also allows for duplicates
  // example:
  //      scala> combinatoricMapping(List(1,2), List('a, 'b))
  //      res0: Set[Map[Symbol,Int]] = Set(Map('a -> 1, 'b -> 1), Map('a -> 2, 'b -> 1), Map('a -> 1, 'b -> 2), Map('a -> 2, 'b -> 2))
  def combinatoricMapping[S,K](src: Iterable[S], keys: Iterable[K]): Set[Map[K,S]] =
    keys.foldLeft(Set(Map.empty[K,S])) { case (acc, key) =>
      src.map(el => acc.map(_ + (key -> el))).reduce(_++_)
    }
  
  // TODO can dimensions occur multiple times????
  // in a minimal solution, no!
  //
  // to play around with it:
  //     object m extends Minimality[Int] with Choices[Int] with CCRecovery[Int]
  //     import m._
  //     allChoiceShapes(('A,Set('a,'b)) :: ('B, Set('a,'b,'c)) :: Nil)
  def allChoiceShapes(dims: Seq[(Symbol, Set[Symbol])]): Set[ChoiceShape] = dims match {
    case Nil => Set(Hole)
    case (dim, tags) :: rest => {
      val shapes = allChoiceShapes(rest).view;
      for (mapping <- combinatoricMapping(shapes, tags))
        yield ChoiceNode(dim, mapping)  
    }
  }
  
  // Example 2 variables 2 values
  // Table
  //   x  y
  //   1  1
  //   1  2
  //   2  1
  //   2  2
  //
  // 1 dim (min 2 tags, since min(|values|) = 2)
  //  1 dim 2 tags
  //  1 dim 3 tags
  //  1 dim 4 tags
  //
  // 2 dim
  //  2 dim á 2 tags
  //  ...
  //
  
  
  // better:
  //   since our first creterion for minimality is leaf size we try to find
  //   solutions by splitting leafs (adding redundancy) and use the coloring
  //   algorithm (adding redundancy at most as rows with this value occur?)
  
  
  // without redundancy
  //   2 values each (x.1,x.2,y.1,y.2)
  //   - 1 dim with 2 tags: A<1,2> A<1,2> | A<2,1> A<1,2> | A<1,2> A<2,1> | A<2,1> A<2,1>
  //   - 2 dims with 2 tags: A<1,2> B<1,2> | A<2,1> B<1,2> | A<1,2> B<2,1> | A<2,1> B<2,1>
  //
  // adding 1 redundancy
  //  doubling x.1 (x.1,x.1,x.2,y.1,y.2)
  //  - 2 dims with 2 tags 
  //    should not generate: A<A<1,1>, 2>> | A<1, A<1,2>>
  //                    but: A<B<1,1>, 2>> | A<1, B<1,2>>
  //  - 1 dim with 3, one dim with 2 tags
  //    should not generate: A<1,1,2> B<1,2>, since this is the same as
  //                         A<1,2> B<1,2>
  //
  // adding 2 redundancy
  // 1. doubling x.1 and x.2 (x.1,x.1,x.2,x.2,y.1,y.2)
  // 2. doubling x.1 and y.1 (x.1,x.1,x.2,y.1,y.1,y.2)
  // 3. doubling x.1 and y.2
  // 4. doubling x.2 and y.1
  // ...
  def generateAllSolutions(implicit table: Table[Symbol, T]): Seq[Solution] = 
    ???
  
  // if given solution is not minimal a counter example is provided
  def isMinimal(sol: Solution)(implicit table: Table[Symbol, T]): Option[Solution] =
    minimalSolution match {
      case min if SolutionOrdering.lteq(sol, min) => None
      case min => Some(min)
    }
  
}
