package choicecalculus
package semantics


// This is something like a "typechecker" only for dimensions

// Requirements for the dimension checker:
// 1. every occurence of a choice is bound by a proper dimension declaration
// 2. a set of dimensions can be assigned to every piece of code
/**
   * Important terminology:
   * 1. Well Formed: The expression does not contain unbound choices (and all choices have correct number of alternatives)
   * 2. Fully Configured: All choices have been resolved by selections.
   */
trait DimensionChecker { // self: ChoiceGraph =>
  
  import ast._
  import org.kiama.attribution.Attribution._
  import org.kiama.rewriting.Rewriter.{rule, sometd, reduce, oncetd, Strategy, topdown}  
  
  /**
   * We have to deal with two different reductions here
   * 1. reducing `select(dim(choice(...)))` to the choice's body
   * 2. reducing `share v = some in body` to body[v→some]
   * 
   * In order to select, all substitutions have to take place first. A "call by name" strategy might lead to too large
   * ASTs since the common subexpressions are inserted without performing selection on them first.
   * 
   * Can we use kiama's strategies to do a more controlled reduction like:
   * 
   * def substitute(env: Map[Symbol, ASTNode]) = rule {
   *   case ShareExpr(name, binding, body) => 
   *   case IdExpr(name) if env contains name => env(name)
   *   case e:IdExpr => sys error "cannot resolve %s".format(e)
   * }
   * 
   * def performSelections
   * 
   */
  /*
  class Preprocessor extends TypeSystem {
    
    import scala.collection.mutable
    
    val fileTypes: mutable.Map[String, Set[DimensionType]] = mutable.Map()
    
    
  }*/
  
  /**
   * Revised algo
   * 
   * 
   * case class Select(tag: Symbol, consumed: Boolean = false)
   * 
   * def preprocess(e: ASTNode, shareEnv: Map[Symbol, ASTNode], selects: Map[Symbol, Select])
   *   
   *   case SelectExpr(dim, tag, body) => preprocess(body, shareEnv, selects + (dim -> Select(tag, false))
   *   case DimensionExpr(dim, tags, body) if selects contains dim => selects(dim) match {
   * 
   *     // This is the first dimension after the select occurred that matches the dimension name.
   *     case Select(tag, false) => preprocess(body, shareEnv, selects + (dim -> Select(tag, true)))
   *     case _ => DimensionExpr(dim, tags, preprocess(body, shareEnv, selects - dim)
   *   } 
   *   // otherwise this dimension has not been selected, or it is an inner one -> rebuild it (this is done in the generic step)
   * 
   *   case ShareExpression(name, binding, body) => preprocess(body, shareEnv + (name -> preprocess(binding, shareEnv, selects)), selects)
   *   
   */
  
  
  /**
   * 
   * 
   * eval(e, someEnv) {
   *   // discussed in "Sharing and Thunks"
   *   case ShareExpression(name, binding, body) if binding->dimension contains free choices => ShareExp(name, binding, eval(body, someEnv))
   * }
   * 
   * 
   */
  
  
  
  
  
  
  
  
  
  
  
  // this may be problematic, since it introduces dynamic scope for dimensions.
  def substitute(env: Map[Symbol, ASTNode]): Strategy = topdown(rule {
    case ShareExpr(name, binding, body) => substitute(env)(binding) match {
      case substBinding:ASTNode => substitute(env + (name -> substBinding))(body)
    }
    case IdExpr(name) if env contains name => env(name)
    case e:IdExpr => sys error "cannot resolve %s".format(e)
  })
  
  
  /*
  def myEval(e: ASTNode, env: Map[Symbol, ASTNode]): ASTNode = e match {
    
    case ShareExpr(name, binding, body) => myEval(body, env + (name -> myEval(binding, env)))
    
    /
    case ChoiceExpr(dim, choices) if (sels contains dim) => choices.collectFirst {
        case Choice(tag, body) if tag == sels(dim) => myEval(body, env, sels - dim)
      } match {
        case Some(e) => e
        case None => sys error "no matching choice found for selection %s.%s".format(dim, sels(dim))
      }
    /
    case ChoiceExpr(dim, choices) => ChoiceExpr(dim, choices.map {
      // attention: We do a type cast here. Maybe this has to be handled differently
      case Choice(tag, body) => Choice(tag, myEval(body, env).asInstanceOf[Expression])
    })
    
    case SelectExpr(dim, tag, body) => myEval(doSelection(dim, tag, body), env)
    
    case DimensionExpr(dim, tags, body) => DimensionExpr(dim, tags, myEval(body, env).asInstanceOf[Expression])
    
    case IdExpr(name) => if (env contains name)
        env(name)
      else
        sys error "unresolved identifier %s".format(name)
    
    case u@UnaryExpr(body) => u.rebuild(myEval(body, env))
    case b@BinaryExpr(lhs, rhs) => b.rebuild(myEval(lhs, env), myEval(rhs, env))
    case other => other
  }*/
  
  /*def substitute(name: Symbol, value: ASTNode) = rule {
    case IdExpr(n) if n == name => value    
  }*/
  
  def doSelection(dim: Symbol, tag: Symbol, body: ASTNode) =
    sometd( selectFromDimension(dim, tag) )(body).get
  
  
    
  /**
   * Due to wellformedness every choice has to be lexically nested inside of a dimension declaration
   * So we first remove that dimension declaration once
   */
  def selectFromDimension(dim: Symbol, tag: Symbol) = rule {    
    // it could also be checked whether tag occurs in `tags`
    case DimensionExpr(d, tags, body) => sometd( selectFromChoices(dim, tag) )(body).get    
  }
  def selectFromChoices(dim: Symbol, tag: Symbol) = rule {
    
    case ChoiceExpr(d, choices) if d == dim => choices.filter {
        case Choice(t, body) => t == tag
      } match {
        case matches if matches.size == 1 => matches(0).body
        case _ => "Cannot select from choice because a wrong number of tags matched"
      }
  }
  
  /**
   * Maybe we should use oncetd here, since merging is currently not supported
   */
  def selectReduction = rule {
    // We apply selectFromDimension to every branch until it works (at any level)
    case SelectExpr(dim, tag, body) => sometd( selectFromDimension(dim, tag) )(body).get
  }
  
  def eval = reduce ( selectReduction )
  /*
  case class Dimension(name: Symbol, tags: Set[Symbol]) {
    def isWildcard = tags.contains('_)
    override def toString = "%s<%s>".format(name.name, tags.map(_.name) mkString ", ")
  }
  object WildcardDim {
    def unapply(dim: Dimension): Option[(Symbol, Set[Symbol])] = 
      if (dim.isWildcard)
        Some( (dim.name, dim.tags) )
      else
        None
  }
 
  val env: ASTNode => List[DimensionExpr] = attr {
    case p if p.isRoot => List ()
    case n => (n.parent) match {
      case p@DimensionExpr(name, tags, body) => p :: p->env
      case p:ASTNode => p->env
    }
  }
  
  val dim: Choice => Symbol = attr {
    case c: Choice => c.parent[ChoiceExpr].dim
  }    
  
  val bindingInstance: ChoiceExpr => DimensionExpr = attr {
    case p => (p->env).find { _.name == p.dim } match {
      case Some(d:DimensionExpr) => d
      case None => sys error "Free choice : %s!".format(p)
    }
  }
  
  val boundInstances: DimensionExpr => List[ChoiceExpr] = attr {
    case p => p.children.collect {
      case c:ChoiceExpr if c->bindingInstance == p => c
    }.toList
  }
  */
}