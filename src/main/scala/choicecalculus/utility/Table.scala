package choicecalculus
package utility

// TODO implement hashCode
// could use shapeless' sized lists to statically check columnsize
class Table[S, T](val headers: S*) {

  var _rows: Set[List[T]] = Set.empty

  def columns: List[List[T]] = _rows.toList.transpose
  def rows: Set[List[T]] = _rows
  def rowsWithHeaders: Set[List[(S, T)]] = rows.map( cs => (headers zip cs).toList )
  
  
  def addRow(els: T*): this.type =
    if (els.size == headers.size) {
      _rows = _rows + els.toList
      this
    } else
      sys error "row does not have the correct size"


  def addRows(rows: List[List[T]]): this.type = {
    for (row <- rows)
      addRow(row: _*)
      
    this
  }

  protected class RowBuilder(els: List[T]) {
    def |() { addRow(els: _*) }
    def |(el: T) = new RowBuilder(els:+(el))
  }
  
  def |(el: T) = new RowBuilder(List(el))
  
  def canEqual(other: Any) = other.isInstanceOf[Table[S, T]]
  
  def toMap: Map[S, List[T]] = Map((headers zip columns):_*)
  
  private def normalizedRows(
    left: Map[S, List[T]], 
    right: Map[S, List[T]]): (Set[Iterable[T]], Set[Iterable[T]]) = {
    // eliminate column ordering
    val both = left.keys.map { key => (left(key), right(key)) }
    
    val leftColNorm = (both map (_._1)).transpose.toSet
    val rightColNorm = (both map (_._2)).transpose.toSet
    
    (leftColNorm, rightColNorm)
  }
  
  // equal meta data, such as header size and names
  private def equalColumnMeta(that: Table[S,T]): Boolean = 
    this.headers.size == that.headers.size &&
    this.headers.toSet == that.headers.toSet

  // equal meta data, such as header size and names
  private def equalRowMeta(that: Table[S,T]): Boolean = 
    this.rows.size == that.rows.size
  
  def subsetOf(that: Table[S,T]): Boolean = this.equalColumnMeta(that) && 
    (normalizedRows(this.toMap, that.toMap) match {
      case (l, r) => l.subsetOf(r)
    })
  
  override def equals(other: Any) = other match {
  
    case other: Table[S, T] if other.canEqual(this) && 
                               this.equalColumnMeta(other) && 
                               this.equalRowMeta(other) => 
      normalizedRows(this.toMap, other.toMap) match {
        case (l, r) => l == r
      }    
    case _ => false
  }
  
  override def toString: String = {
    // find column sizes
    val sizes = columns.map { _.map { _.toString.size }.max }
    
    rows.map { row =>
      (row zip sizes).map { case (col, size) =>
        s"| %${size}s ".format(col.toString)
      }.mkString("")
    }.mkString("|\n") + "|\n"
  }
}
