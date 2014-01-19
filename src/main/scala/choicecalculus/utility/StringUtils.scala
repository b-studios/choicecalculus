package choicecalculus
package utility

import util.matching.Regex
import scala.Console.{ BOLD, YELLOW, RED, WHITE, RESET }

package object strings {

  implicit class StringOps(text: String) {

    def stripLinebreaks: String = text.stripMargin.replaceAll("\n", "")

    // ANSI Colors
    def bold: String = BOLD + text + RESET
    def yellow: String = YELLOW + text + RESET
    def white: String = WHITE + text + RESET
    def red: String = RED + text + RESET

    // Escaping / Unescaping
    def safeUnescape: String = 
      safeChars.replaceAllIn(text, { m => safeToUnsafe(m.group(0)) })

    def safe: String = 
      unsafeChars.replaceAllIn(text, { m => unsafeToSafe(m.group(0)) })

    def escape: String = xml.Utility.escape(text)
    
    private lazy val unsafeChars = new Regex(s"([${unsafeToSafe.keys mkString ""}])")
    private lazy val safeChars = new Regex(s"([${safeToUnsafe.keys mkString ""}])")

    private lazy val safeToUnsafe = Map(
      "《" -> "<",
      "》" -> ">",
      "ᑊ" -> "\"",
      "ꝫ" -> "&"
    )

    private lazy val unsafeToSafe = safeToUnsafe.map(_.swap)
  }
}