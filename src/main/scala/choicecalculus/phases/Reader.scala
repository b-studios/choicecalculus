package choicecalculus
package phases

import org.kiama.attribution.Attribution.{ attr, initTree }
import org.kiama.rewriting.Rewriter.{ everywheretd, query }
import org.kiama.util.IO

import lang.trees.{ Include, Tree }

import scala.collection.mutable

import utility.messages._

import java.io.{ File, BufferedReader, StringReader, FileNotFoundException }

/**
 * <h2>The Reader phase
 *
 * The reader phase takes a given filename and reads it's contents
 * using a parser.
 *
 * The reader phase is not part of the ast pipeline since it works
 * on multiple asts at the same time. It is part of the driver
 * managing the overall processing. This way it is also possible to
 * maintain a global cache of processed files when dealing with
 * multiple inputs.
 *
 * All file names are resolved relative to the current working directory.
 */
trait Reader { self: Parser =>

  /**
   * Returns the sourcefile for `input` and it's dependencies as a pair.
   *
   * The dependencies are presented in topological order
   */
  def runReader(input: String): (SourceFile, List[SourceFile]) =
    messageScope(phase = 'reader) {
      val source = lookupSourceFile(input)
      val tree = source readWith parsers.topLevel

      (source, tree->collectSourceFiles)
    }

  /**
   * Returns the sourcefile for `tree` and it's dependencies as a pair.
   *
   * The dependencies are presented in topological order. This overloaded
   * version of `runReader` creates a virtual file for the given tree instead
   * of parsing a source file.
   */
  def runReader(tree: Tree): (SourceFile, List[SourceFile]) =
    messageScope(phase = 'reader) {
      val source = VirtualFile(tree)
      (source, tree->collectSourceFiles)
    }

  /**
   * Resets the file cache
   */
  def resetReader() { _cache.clear() }

  /**
   * This is used by the REPL. 
   *
   * Creates a virtual file using the given contents
   *
   * @return the name of the virtual file
   */
  def createVirtualFile(contents: String): String = {
    val tree = parsers.parseFile(parsers.topLevel, new StringReader(contents))
    val file = VirtualFile(tree)
    _cache update(file.filename, file)
    file.filename
  }


  /**
   * Returns the tree that will be included
   */
  lazy val tree: Include => Tree = attr {
    case inc @ Include(filename, parser: TreeParser) => lookupSourceFile(filename) readWith parser
  }

  /**
   * Lookup a SourceFile by name. Make sure to run the reader phase
   * before using the lookup, since parts of SourceFile are only
   * initialized during the reader phase.
   */
  def lookupSourceFile(filename: String): SourceFile = {
    val file = new File(filename)
    _cache getOrElseUpdate (file.getCanonicalPath, RealFile(file))
  }

  /**
   * Represents source files that have been parsed
   *
   * A SourceFile can have multiple ASTs depending on the
   * parser that has been used to construct the ast.
   *
   * For instance the javascript expression `3+4` can both
   * be parsed as "expression" as well as "statement".
   *
   * This seems like a slight misconception since a sourcefile
   * can only have one output.
   */
  trait SourceFile {

    /**
     * The parsed trees.
     *
     * It's multiple ones since depending on the
     * context of include the file can be parsed with different
     * parsers.
     */
    def trees: Seq[Tree]

    /**
     * The canonical name of the file
     */
    def filename: String

    /**
     * Reads the file the given parser
     */
    def readWith(p: TreeParser): Tree
  }

  private case class RealFile(file: File) extends SourceFile {

    def trees: Seq[Tree] = _parsers.values.toSeq

    def filename: String = file.getCanonicalPath

    def readWith(p: TreeParser): Tree =
      messageScope(filename = filename) {
        _parsers getOrElseUpdate (p, {
          val tree = parsers.parseFile(p, contents)
          initTree(tree)
          tree;
        })
      }

    /**
     * The textual contents of the file
     */
    private def contents: BufferedReader = try {
      IO.filereader(filename)
    } catch {
      case _: FileNotFoundException | _: IO.FileNotFoundException =>
        raise(s"File not found: $filename")
    }

    private val _parsers = mutable.Map[TreeParser, Tree]()
  }

  private case class VirtualFile(tree: Tree) extends SourceFile {

    initTree(tree)

    val trees: Seq[Tree] = Seq(tree)

    // Generate a random filename for this virtual file
    lazy val filename: String =
      new File("virtual-" + java.util.UUID.randomUUID().toString()).getCanonicalPath

    def readWith(p: TreeParser): Tree = tree
  }

  private case class DummyFile(pos: Tree) extends SourceFile {
    private def cyclic: Nothing = raise(s"Cyclic dependency", position = pos)
    def trees = cyclic
    def filename = cyclic
    def readWith(p: TreeParser): Tree = cyclic
  }

  // map from canonical path to sourcefile
  private val _cache = mutable.HashMap[String, SourceFile]()

  /**
   * attr to collect source files and trigger parsing of these
   *
   * @example {{{
   *   // file1.js
   *   include "file2.js"
   *   include "file3.js"
   *
   *  // file2.js
   *  include "file3.js"
   *
   *  // file3.js
   *  ...
   *
   *  file1Ast->collectSourceFiles
   *  // => File3 :: File2 :: File1 :: Nil
   * }}}
   *
   *
   * kiama's `collectall` is not released, yet. So we have to use
   * attributes and queries.
   *
   * @return a topological order of dependencies
   */
  private lazy val collectSourceFiles: Tree => List[SourceFile] = attr {
    case node =>

      var dependencies = List.empty[SourceFile]

      // strategy to collect dependencies
      val collect = query {

        case inc @ Include(filename, parser: TreeParser) => {

          val file = new File(filename)
          val path = file.getCanonicalPath

          dependencies = dependencies ++ (_cache get (path) match {

            // Already processed, so it's dependencies are already resolved 
            // Still requires processing of the possibly new tree
            // (This is due to the fact, that `SourceFile`s can have
            // multiple trees, depending on the used parser)
            case Some(source) => (source readWith parser)->collectSourceFiles

            // Put a dummy if SourceFile hasn't been processed, yet.
            // Then resolve dependencies of `source` before updating
            // the cache
            case None => {

              _cache update (path, DummyFile(inc))
              val source = RealFile(file)
              val deps = (source readWith parser)->collectSourceFiles
              _cache update (path, source)

              deps :+ source
            }
          })
        }
      }

      everywheretd(collect)(node)

      dependencies.distinct
  }

}