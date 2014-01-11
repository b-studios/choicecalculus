package choicecalculus.lang
package choicecalculus

trait ChoiceCalculusPP extends PrettyPrinter {

  val cc_prefix = text("#")

  override def toDoc(e: ASTNode): Doc = e match {

    case Dimension(dim, tags, body) =>
      "dim" <+> text(dim.name) <> parens(fillsep(tags.map((t) => text(t.name)), comma)) <+>
        "in" <+> toDoc(body)

    case Choice(dim, alts) =>
      "choice" <+> text(dim.name) <+>
        braces(nest(line <> ssep(alts.map(toDoc), line)) <> line)

    case Alternative(tag, body) =>
      "case" <+> text(tag.name) <+> "=>" <+> toDoc(body)

    case Select(dim, tag, body) =>
      "select" <+> text(dim.name) <> dot <> text(tag.name) <+> "from" <+> toDoc(body)

    case Identifier(id) =>
      cc_prefix <> text(id.name)

    case Share(x, binding, body) =>
      "share" <+> text(x.name) <+> "as" <+> toDoc(binding) <+> "within" <+> toDoc(body)

    case PartialConfig(body, configs) =>
      configs.foldLeft(toDoc(body)) {
        (old, config) => "select" <+> text(config._1.name) <> dot <> text(config._2.name) <+> "from" <+> old
      }

    case Include(filename, _) =>
      "include" <+> surround(filename, '"')

    case other => super.toDoc(other)
  }
}