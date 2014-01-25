package choicecalculus
package utility

import lang.PrettyPrinter
import lang.trees.Tree

import org.kiama.output.{ Side, NonAssoc, LeftAssoc, RightAssoc,
                          Fixity, Postfix, Prefix, Infix }

/**
 * Based on kiama's ParenPrettyPrinter. See the original copyright below
 */

 /*
 * This file is part of Kiama.
 *
 * Copyright (C) 2011-2014 Anthony M Sloane, Macquarie University.
 *
 * Kiama is free software: you can redistribute it and/or modify it under
 * the terms of the GNU Lesser General Public License as published by the
 * Free Software Foundation, either version 3 of the License, or (at your
 * option) any later version.
 *
 * Kiama is distributed in the hope that it will be useful, but WITHOUT ANY
 * WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public License for
 * more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with Kiama.  (See files COPYING and COPYING.LESSER.)  If not, see
 * <http://www.gnu.org/licenses/>.
 */
trait ParenPrettyPrinter { self : PrettyPrinter =>

    def toDoc(e: Tree): Doc

    def priorityOf(e: Tree): Int
    def fixityOf(e: Tree): Fixity
    def mightRequireParens(e: Tree): Boolean

    def toParenDoc(parent: Tree, children: Tree*)(constr: List[Doc] => Doc): Doc = 
      toParenDoc(parent, children.toList)(constr)

    def toParenDoc(parent: Tree, children: List[Tree])(constr: List[Doc] => Doc): Doc = children match {
      case Nil => constr(Nil)
      case first :: Nil => constr(bracket(first, parent, NonAssoc) :: Nil)
      case many => constr(many.map { bracket(_, parent, LeftAssoc) })
      // case many => {
      //   val firstDocs = many.init.map { bracket(_, parent, LeftAssoc) }
      //   val lastDoc = bracket(many.last, parent, RightAssoc)
      //   constr(firstDocs :+ lastDoc)
      // }
    }

    /**
     * Optionally parenthesize an operator expression based on the precedence relation
     * with an outer expression's operator.
     */
    def bracket(inner: Tree, outer: Tree, side: Side): Doc = {
      if (mightRequireParens(inner) && !noparens (inner, outer, side))
        parens(toDoc(inner))
      else
        toDoc(inner)
    }

    /**
     * Return true if the inner expression should be parenthesized when appearing
     * on the given side with the outer expression.
     */
    def noparens (inner: Tree, outer: Tree, side: Side): Boolean = {
      val pi = priorityOf(inner)
      val po = priorityOf(outer)
      lazy val fi = fixityOf(inner)
      lazy val fo = fixityOf(outer)
      (pi > po) ||
        ((fi, side) match {
          case (Postfix, LeftAssoc) => true
          case (Prefix, RightAssoc) => true
          case (Infix (LeftAssoc), LeftAssoc) => (pi == po) && (fo == Infix (LeftAssoc))
          case (Infix (RightAssoc), RightAssoc) => (pi == po) && (fo == Infix (RightAssoc))
          case (_, NonAssoc) => fi == fo
          case _ => false
        })
    }

}