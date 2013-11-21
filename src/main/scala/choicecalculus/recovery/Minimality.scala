package choicecalculus
package recovery

import lang.ASTNode
import lang.choicecalculus.{ Choices, Choice }
import scala.collection.SeqView

private[recovery] trait Minimality { self: CCRecovery =>
  
  type Solution = Map[
    Symbol, // Variable name
    ASTNode // Choice calculus expression
  ]

  implicit object SolutionOrdering extends Ordering[Solution] {
    def compare(a: Solution, b: Solution) = 
      (numberOfLeafs(a) compare numberOfLeafs(b)) match {
        case 0 => numberOfDims(a) compare numberOfDims(b)
        case n => n      
      }
  }
  
  def isSolution(sol: Solution)(implicit table: CloneInstanceTable): Boolean =
    tableFromChoices(sol) == table
  
  def minimalSolution(implicit table: CloneInstanceTable): Solution = {
    generateAllSolutions.filter(isSolution).sorted.head
  }


  private def tagLimit(table: CloneInstanceTable): Int = 
    table.rows.size

  // TODO check if this is true
  private def dimLimit(table: CloneInstanceTable): Int = 
    Math.ceil(Math.log(table.rows.size * table.columns.size) / Math.log(2)).toInt
  
  private def numberOfLeafs(sol: Solution): Int = 
    sol.map { case (_, c) => numberOfLeafs(c) }.sum
    
  private def numberOfLeafs(sol: ASTNode): Int = sol match {
    case Choices(dim, cases) => cases.map { 
      case Choice(_, c) => numberOfLeafs(c)
    }.sum
    
    case _ => 1
  } 
  
  private def numberOfDims(sol: Solution): Int = 
    sol.flatMap { 
      case (_, c) => collectDims(c) 
    }.toSet.size
    
  private def collectDims(sol: ASTNode): Set[Symbol] = sol match {
    case Choices(dim, cases) => Set(dim) ++ cases.flatMap { 
      case Choice(_, c) => collectDims(c)
    }.toSet
    
    case _ => Set.empty
  }
  
  /**
   * Local representation of choice shapes. A shape can either be a node or a hole
   */
  private[this] trait ChoiceShape
  private[this] case class ChoiceNode(dim: Symbol, choices: Map[Symbol, ChoiceShape]) extends ChoiceShape {
    override def toString: String = s"$dim<$choices>"
  }
  private[this] case object Hole extends ChoiceShape {
    override def toString: String = "☐"
  }
  
  // does not contain duplicates like a -> 1, b -> 1
  // example: 
  //     scala> combinatoricMapping(List(1,2,3), List('a,'b))
  //     res0: Set[Map[Symbol,Int]] = Set(Map('a -> 1, 'b -> 2), Map('a -> 1, 'b -> 3), Map('a -> 2, 'b -> 3))
  private[this] def combinatoricMappingNoDup[S,K](src: Iterable[S], keys: Iterable[K]): Set[Map[K,S]] =
    src.toList.combinations(keys.size).map(c => (keys zip c).toMap).toSet

  // This implementation also allows for duplicates
  // example:
  //      scala> combinatoricMapping(List(1,2), List('a, 'b))
  //      res0: Set[Map[Symbol,Int]] = Set(Map('a -> 1, 'b -> 1), Map('a -> 2, 'b -> 1), Map('a -> 1, 'b -> 2), Map('a -> 2, 'b -> 2))
  private[this] def combinatoricMapping[S,K](src: Iterable[S], keys: Iterable[K]): Set[Map[K,S]] =
    keys.foldLeft(Set(Map.empty[K,S])) { case (acc, key) =>
      src.map(el => acc.map(_ + (key -> el))).reduce(_++_)
    }
  
  // Usage:
  //     allChoiceShapes(('A,Set('a,'b)) :: ('B, Set('a,'b,'c)) :: Nil)

  // TODO check whether allChoiceshapes also include just the empty leaf
  private[this] def allChoiceShapes(dims: Seq[(Symbol, Set[Symbol])]): Set[ChoiceShape] = dims match {
    case Nil => Set(Hole)
    case (dim, tags) :: rest => {
      val shapes = allChoiceShapes(rest).view;

      Set(Hole) ++ // empty
      shapes    ++ // without dim
      (for (mapping <- combinatoricMapping(shapes, tags)) // with dim
        yield ChoiceNode(dim, mapping))
    }
  }

  private[this] def allAssignments(shapes: Set[ChoiceShape], values: List[ASTNode]): Set[ASTNode]
    = shapes.flatMap(allAssignments(_, values))

  private[this] def allAssignments(shape: ChoiceShape, values: List[ASTNode]): Set[ASTNode] =
    shape match {

      case Hole => values.toSet

      case ChoiceNode(dim, choices) =>
        // recursively apply to subshapes
        choices.map { case (tag, c) => 
          (tag, allAssignments(c, values)) 

        // combine to set of maps
        }.foldLeft(Set(Map.empty[Symbol, ASTNode])) { case (acc, (tag, partialSol)) =>
          partialSol.flatMap(s => acc.map(_ + (tag -> s)))
        }.map { case m =>
          Choices(dim, m.toList.map { case (tag, v) => Choice(tag, v) })
        }
    }

  // maybe remove, or call allTrees
  def allSolutionCandidates(dims: Seq[(Symbol, Set[Symbol])], values: List[ASTNode]): Set[ASTNode] =
    allAssignments(allChoiceShapes(dims), values)

  // TODO turn into View
  def allDimensions(maxDims: Int, maxTags: Int): Seq[Seq[Dimension]] = 
    (1 to maxDims).flatMap { dimCount =>
      val dimNames = (1 to dimCount).map { number => Symbol(s"d_$number") }
      combinatoricMapping(allTags(maxTags), dimNames).map(_.toList)
    }

  // if limit is 4 then
  // [{t_1,t_2}, {t_1,t_2,t_3}, {t_1,t_2,t_3,t_4}]
  def allTags(maxTags: Int): Seq[Set[Symbol]] =
    for {
      i <- (2 to maxTags)
    } yield (1 to i).map( n => Symbol(s"t_$n")).toSet

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
  def generateAllSolutions(implicit table: CloneInstanceTable): Seq[Solution] = { 
    val dims = allDimensions(dimLimit(table), tagLimit(table))
    (for {
      (col, name) <- table.columns.zip(table.headers)
      values = col.distinct
      candidate <- dims.map(dims => allSolutionCandidates(dims, values))

    // reduce as cross product
    } yield (name, candidate)).foldLeft(Set(Map.empty[Symbol, ASTNode])) {
      case (acc, (name, sols)) =>
        // TODO extract this pattern!
        sols.map(sol => acc.map(_ + (name -> sol))).reduce(_++_)
    }.toSeq
  }
}
