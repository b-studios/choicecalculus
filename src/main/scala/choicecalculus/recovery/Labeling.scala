package choicecalculus
package recovery

private[recovery] object labeling {

  /**
   * Using a local representation for choices on paths just consisting of
   * the dimension and tag. For instance `('A, 'b)` represents the choice
   * of tag `b` on dimension `A`.
   */
	private[this] type Choice = (Symbol, Symbol)

  private val dummyChoice = (Symbol(""), Symbol(""))

  /**
   * A path is basically a list of choices like `A.b :: B.a :: Nil`
   */
	case class Path(var choices: List[Choice] = Nil) {
	  	  
	  // prefix
	  def ::(c: Choice) = Path(c :: choices)
	  
	  // suffix
	  def :+(c: Choice) = Path(choices:+(c))
	
    def cloneWithTagReplaced(dim: Symbol, newTag: Symbol) = Path(choices.map {
      case (d, _) if d == dim => (d, newTag)
      case other => other
    })

	  def contains(c: Choice): Boolean = choices.contains(c)
	
    def containsChoiceFor(dim: Symbol) = choices.exists { case (d, _) => d == dim }
        
    def disjoint(other: Path): Boolean = disjoint(this.choices, other.choices)
    
    // can p1 and p2 be selected at the same time?
    // whats with A1 and B1-A2
    private def disjoint(p1: List[Choice], p2: List[Choice]): Boolean = (p1, p2) match {
      case (Nil, _) => false
      case (_, Nil) => false
      case ((dim1, tag1) :: r1, (dim2, tag2) :: r2) =>
        if (dim1 == dim2) 
          tag1 != tag2 || disjoint(r1, r2)
        else 
          disjoint(r1, p2) && disjoint(p1, r2)
    }

    // Assumption: dimension only occurs once per path
    def replaceByDummy(dim: Symbol) = 
      choices.foldRight[(Option[Symbol], Path)]((None, Path(Nil))) {
        case ((d, t), (_, cs)) if d == dim => (Some(t), dummyChoice :: cs)
        case (c, (tag, cs)) => (tag, c :: cs)    
      }
    
    def removeChoicesFor(dim: Symbol) = Path(choices.filter { case (d, _) => d != dim })

    def removeDummy: Path = Path(choices.filter { _ != dummyChoice })
    
    def replaceDummyByChoices(dim: Symbol, tags: Set[Symbol]): Set[Path] = tags.map { t =>
      Path(choices.map { 
        case c if c == dummyChoice => (dim, t)
        case c => c
      })
    }

    override def toString: String = 
      choices.map { case (dim, tag) => dim.name + "." + tag.name  }.mkString(" - ")
	}

  // The {Nil} singleton requires special handling when propagating suffixes
  case class Label(var paths: Set[Path] = Set(Path())) {
    
    def addPrefix(dim: Symbol, tag: Symbol) = paths = paths.map { path =>
      (dim, tag) :: path
    }
    
    def addSuffix(dim: Symbol, tag: Symbol) = paths = paths.flatMap { 
      case path if path.containsChoiceFor(dim) => Set(path, path.cloneWithTagReplaced(dim, tag))
      case path => Set(path:+((dim, tag)))
    }
    
    def clearEmptyPaths: Label = Label(paths.filterNot(_ == Path()))
    
    def contains(c: Choice): Boolean =
      paths.exists { _.contains(c) }
    
    def containsChoiceFor(dim: Symbol): Boolean = 
      paths.exists { _.containsChoiceFor(dim) }
  
    def disjoint(other: Label): Boolean = (this eq other) ||
      paths.forall { p1 => 
        other.paths.forall { p2 => p1 disjoint p2 }}    
    
    def removeChoicesFor(dim: Symbol) = Label(paths.map(_.removeChoicesFor(dim)))
      
    def size: Int = paths.size
  
    override def toString: String = 
      paths.mkString(", ")
  }
}
