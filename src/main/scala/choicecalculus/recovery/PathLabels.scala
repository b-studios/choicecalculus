package choicecalculus
package recovery

private[recovery] trait PathLabels { self: Dimensions =>

	type Choice = (Symbol, Symbol)

  val dummyChoice = (Symbol(""), Symbol(""))

	// type Path = List[Choice]
	
	case class Path(var choices: List[Choice] = Nil) {
	  
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
	  
	  // prefix
	  def ::(c: Choice) = Path(c :: choices)
	  
	  // suffix
	  def :+(c: Choice) = Path(choices:+(c))
	
	  def contains(c: Choice): Boolean = choices.contains(c)
	
    def containsChoiceFor(dim: Symbol) = choices.exists { case (d, _) => d == dim }
  
    // Assumption: dimension only occurs once per path
    def replaceByDummy(dim: Symbol) = 
      choices.foldRight[(Option[Symbol], Path)]((None, Path(Nil))) {
        case ((d, t), (_, cs)) if d == dim => (Some(t), dummyChoice :: cs)
        case (c, (tag, cs)) => (tag, c :: cs)    
      }
      
    def cloneWithTagReplaced(dim: Symbol, newTag: Symbol) = Path(choices.map {
      case (d, _) if d == dim => (d, newTag)
      case other => other
    })
    
    def removeChoicesFor(dim: Symbol) = Path(choices.filter { case (d, _) => d != dim })
      
    override def toString: String = 
      choices.map { case (dim, tag) => dim.name + "." + tag.name  }.mkString(" - ")
      
    def removeDummy: Path = Path(choices.filter { _ != dummyChoice })
    
    def replaceDummyByChoices(dim: Symbol, tags: Set[Symbol]): Set[Path] = tags.map { t =>
      Path(choices.map { 
        case c if c == dummyChoice => (dim, t)
        case c => c
      })
    }
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
    
    def expand(dim: Symbol): Label = Label(paths.flatMap {
      case p if p.containsChoiceFor(dim) => Set(p)
      // expansion
      case p => dimensions(dim).map { tag => p:+((dim, tag)) }
    })
    
    def removeChoicesFor(dim: Symbol) = Label(paths.map(_.removeChoicesFor(dim)))
    
    
  /**
   * All variants stripping ({ αA1β, ..., αAkβ } -> {αβ} if A<1,...,k> is defined)
   * Complexity: |dims| * |paths|
   */
    def allVariantsStripping: Unit = 
      for ((dim, tags) <- dimensions) {
      
       paths.map { _.replaceByDummy(dim) }
        .groupBy { case (tag, restPath) => restPath }
        .mapValues { _.collect { case (Some(tag), path) => tag }.toSet }
        .collect {
          case (path, tags) if (dimensions(dim).toSet == tags) =>            
            paths = paths -- path.replaceDummyByChoices(dim, tags)
            paths = paths + path.removeDummy
        }
    
    }
    
    def clearEmptyPaths: Label = Label(paths.filterNot(_ == Path()))
    
    def contains(c: Choice): Boolean =
      paths.exists { _.contains(c) }
    
    def containsChoiceFor(dim: Symbol): Boolean = 
      paths.exists { _.containsChoiceFor(dim) }
  
    def disjoint(other: Label): Boolean = (this eq other) ||
      paths.forall { p1 => 
        other.paths.forall { p2 => p1 disjoint p2 }}
  
    def size: Int = paths.size
  
    override def toString: String = 
      paths.mkString(", ")
  }
}
