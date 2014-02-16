package choicecalculus
package phases

import lang.trees._

import org.kiama.rewriting.Rewriter
import org.kiama.attribution.UncachedAttribution.attr

import utility.messages._

/**
 * <h2> The DimensionChecker phase
 */
trait DimensionChecker { self: Reader with Namer with Rewriter =>

  /**
   * Decorates the given tree with it's dimensioning
   *
   * Before running the dimension checker please assure that
   * the [[Namer]] has been run.
   */
  def runDimensionChecker(ast: Tree): Tree =
    messageScope(phase = 'dimensionchecker) {
      ast->dimensioning
      ast
    }

  /**
   * Computes the dimension dependency graph for a given Tree
   */
  val dimensioning: Tree => DependencyGraph =
    messageScope(phase = 'dimensionchecker) {
      attr {

        case c @ Choice(dim, alts) => alts.foldLeft(DependencyGraph.empty) {
          case (acc, Alternative(tag, body)) =>
            acc.merge((body->dimensioning).fromAlternative(c, tag), c)
        }

        case d @ Dimension(name, tags, body) => (body->dimensioning).fromDimension(d)

        case s @ Select(dim, tag, body) => (body->dimensioning).select(dim, tag, s)

        case id @ Identifier(name) => (id->symbol).definition match {
          case Share(_, boundExpr, _) => boundExpr->dimensioning
        }

        case p @ PartialConfig(body, configs) => (configs.foldLeft(body->dimensioning) {
          case (old, (dim, tag)) => old.select(dim, tag, p)
        })

        case Share(name, boundExpr, body) => body->dimensioning

        case inc: Include => inc->tree->dimensioning

        case t @ Term(p, children) => children.flatMap {
          case n: Tree => List(n->dimensioning)
          case l: Seq[Tree] => l.map(dimensioning)
          case o: Option[Tree] => o.map(dimensioning)
          case _ => List.empty
        }.foldLeft(DependencyGraph.empty) {
          case (old, other) => old.merge(other, t)
        }

        case _ => DependencyGraph.empty
      }
    }


  /**
   * Represents the dependencies between dimensions
   *
   * As opposed to the previous approaches, this representation is centered
   * around the dependent dimension rather then the other way around.
   *
   * <strong>Advantages</strong>:
   * <ul>
   *   <li> Dependencies can be determined directly (No queries necessary to
   *        find a dimension in the graph)
   *   <li> Dimensions with different dependencies are treated not as same
   *   <li> Since it is ast based (as opposed to name based) bindings of
   *        identifiers and choices can be resolved by using symbols (if those
   *        are preserved during rewriting)
   * </ul>
   *
   * <strong>Disadvantages</strong>:
   * <ul>
   *   <li> it cannot be determined whether the dimension is actually used. In
   *        former encodings explicitly listing unbound choices, it could be
   *        determined that at least one choice for a given dimension exists.
   *        (Of course this could still be done using symbols)
   *   <li> implementation of selection on the dependency graph is a little bit
   *        more involved.
   * </ul>
   */
  case class DependencyGraph(val dims: Set[DependentDimension], val choices: Set[Sym] = Set.empty) {

    def fullyConfigured = dims.isEmpty && choices.isEmpty

    def fromDimension(dim: Dimension): DependencyGraph =
      DependencyGraph(dims + DependentDimension(dim), choices - (dim->symbol))

    def fromAlternative(choice: Choice, tag: Symbol): DependencyGraph =
      (choice->symbol).definition match {
        case dim: Dimension => {

          // check whether all alternatives are handled
          val coveredTags = choice.alternatives.map(_.tag).toSet
          val requiredTags = dim.tags.toSet
          val missingTags = requiredTags -- coveredTags
          val superfluousTags = coveredTags -- requiredTags

          if (!missingTags.isEmpty) {
            errors.tagsMissing(dim.name, missingTags, choice)
          } else if (!superfluousTags.isEmpty) {
            warnings.superfluousTags(superfluousTags, choice)
          }

          DependencyGraph(dims.map {
            case DependentDimension(d, deps) =>
              DependentDimension(d, deps + Dependency(dim, tag))
          }, choices + (dim->symbol))
        }
        case _ => errors.noBindingDimension(choice)
      }

    def select(selDim: Symbol, selTag: Symbol, at: Tree): DependencyGraph = selectionCandidates(selDim) match {

      case candidates if candidates.isEmpty => {
        val dependentDims = dimsWithName(selDim)

        if (dependentDims.isEmpty) {
          warnings.vacuousSelection(selDim, selTag, at)
        } else {
          warnings.dependentSelection(selDim, dependentDims, at)
        }
        this
      }

      case candidates => {

        // check whether the selected tag is present. If not raise error
        val selected = candidates.collect {
          case DependentDimension(dim, _) if !(dim.tags contains selTag) =>
            errors.cannotSelectTag(selDim, selTag, at)

          case DependentDimension(dim, _) => Dependency(dim, selTag)
        }

        DependencyGraph(selected.foldLeft(dims -- candidates) {
          case (remaining, sel) => remaining.flatMap { _.resolveDependency(sel) }
        }, choices -- selected.map(_.dim->symbol))
      }
    }

    def merge(that: DependencyGraph, at: Tree): DependencyGraph = {

      // we first compare dims by name, not ast
      val intersection = (this.dims.toList ++ that.dims.toList).groupBy { 
        dep => (dep.dim.name, dep.dependsOn)
      }

      intersection.foreach {
        case (k, deps) if deps.size > 1 =>
          // try to recover by checking whether all dimensions are actually the same:
          if (!deps.forall { x => deps.forall { y => x.dim->symbol eq y.dim->symbol }}) {
            errors.multipleDimensions(k, at)
          }
        case _ =>
      }

      DependencyGraph(this.dims ++ that.dims, this.choices ++ that.choices)
    }

    private def selectionCandidates(dimName: Symbol): Set[DependentDimension] =
      dimsWithName(dimName).filter { _.dependsOn == Set.empty }

    private def dimsWithName(dimName: Symbol): Set[DependentDimension] =
      dims.filter { _.dim.name == dimName }

    override def toString = dims mkString "\n"

    private object warnings {
      def vacuousSelection(dim: Symbol, tag: Symbol, pos: Tree) {
        warn(s"Your selection ${dim.name}.${tag.name} is vacuous.", position = pos)
      }

      def dependentSelection(dim: Symbol, deps: Set[DependentDimension], pos: Tree) {
        warn(s"""You are trying to select a dependent dimension "${dim.name}". Try
                |selecting the enclosing dimensions first, on which "${dim.name}" depends:
                |  $deps""".stripMargin, position = pos)
      }

      def superfluousTags(tags: Set[Symbol], pos: Tree) {
        val formattedTags = tags.map { _.name } mkString ", "
        val alternatives = if (tags.size > 1) "Alternatives" else "Alternative"
        val are = if (tags.size > 1) "are" else "is"
        warn(s"""$alternatives $formattedTags $are not declared in binding dimension""", position = pos)
      }
    }

    private object errors {
      def tagsMissing(dim: Symbol, tags: Set[Symbol], pos: Tree): Nothing = {
        val formattedTags = tags.map { t => s"${dim.name}.${t.name}" } mkString ", "
        raise(s"""Missing alternatives: $formattedTags""", position = pos)
      }

      def cannotSelectTag(dim: Symbol, tag: Symbol, pos: Tree): Nothing =
        raise(s"Cannot select ${dim.name}.${tag.name} because this tag is not declared", position = pos)

      def noBindingDimension(pos: Tree): Nothing =
        raise(s"Could not resolve binding dimension for choice", position = pos)

      def multipleDimensions(conflicting: (Symbol, Set[Dependency]), pos: Tree): Nothing =
        raise(s"Multiple dimension declarations at one level: $conflicting", position = pos)
    }
  }

  object DependencyGraph {
    def empty = new DependencyGraph(Set.empty, Set.empty)
  }

  case class DependentDimension(
      dim: Dimension,
      dependsOn: Set[Dependency] = Set.empty) {

    /**
     * Some dependency might have been selected
     */
    def resolveDependency(dep: Dependency): Option[DependentDimension] = dep match {
      case Dependency(selDim, selTag) =>
        // If this dimension depends on a different tag => no way to be selected
        if (dependsOn.exists { d => d.dim == selDim && d.tag != selTag }) {
          None
        } else {
          Some(DependentDimension(dim, dependsOn.filterNot { _.dim == selDim }))
        }
    }

    override def toString =
      s"""${dim.name.name} --> {${dependsOn mkString ", "}}"""
  }

  case class Dependency(dim: Dimension, tag: Symbol) {
    override def toString = s"${dim.name.name}.$tag"
  }
}