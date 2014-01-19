package choicecalculus
package lang.lccc

import trees._
import lang.trees._

/**
 * A pretty printer for the LcCc language
 */
trait PrettyPrinter extends lang.PrettyPrinter {

  override def toDoc(e: Tree): Doc = e match {

    // Lambda Calculus
    case Lambda(arg, body) =>
      "\\" <> toDoc(arg) <> "." <+> toDoc(body)

    case App(fun, arg) =>
      toDoc(fun) <+> toDoc(arg)

    case BinaryOp(lhs, op, rhs) =>
      toDoc(lhs) <+> text(op) <+> toDoc(rhs)

    case Literal(content) =>
      text(content)

    case Grouping(tree) =>
      parens(toDoc(tree))


    // Choice Calculus
    case Dimension(dim, tags, body) =>
      "dim" <+> text(dim.name) <> angles(fillsep(tags.map((t) => text(t.name)), comma)) <+>
        "in" <+> toDoc(body)

    case Choice(dim, alts) =>
      text(dim.name) <+> angles(ssep(alts.map(toDoc), ", "))

    case Alternative(tag, body) =>
      text(tag.name) <+> ":" <+> toDoc(body)

    case Select(dim, tag, body) =>
      "select" <+> text(dim.name) <> dot <> text(tag.name) <+> "from" <+> toDoc(body)

    case Identifier(id) =>
      "#" <> text(id.name)

    case Share(id, binding, body) =>
      "share" <+> "#" <> text(id.name) <+> "=" <+> toDoc(binding) <+> "in" <+> toDoc(body)

    case PartialConfig(body, configs) =>
      configs.foldLeft(toDoc(body)) {
        (old, config) => "select" <+> text(config._1.name) <> dot <> text(config._2.name) <+> "from" <+> old
      }

    case Include(filename, _) =>
      "include" <+> surround(filename, '"')
  }
}
object PrettyPrinter extends PrettyPrinter {
  implicit val pp = this
}