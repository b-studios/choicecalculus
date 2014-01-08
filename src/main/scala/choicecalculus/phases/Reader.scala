package choicecalculus
package phases

import org.kiama.attribution.Attribution.{ attr, initTree }
import org.kiama.rewriting.Rewriter.{ everywheretd, query }
import org.kiama.util.IO

import lang.ASTNode
import lang.choicecalculus.Include

import scala.collection.mutable

import java.io.File
import java.io.BufferedReader

/**
 * <h1>The Reader phase
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
  def runReader(input: String): (SourceFile, List[SourceFile]) = {
    val source = lookupSourceFile(input)
    val tree = source readWith parsers.topLevel

    (source, tree->collectSourceFiles)
  }

  /**
   * Returns the tree that will be included
   */
  lazy val tree: Include[_,_] => ASTNode = attr {
    case inc@Include(filename, parser: TreeParser) => lookupSourceFile(filename) readWith parser
  }

  /**
   * Lookup a SourceFile by name. Make sure to run the reader phase
   * before using the lookup, since parts of SourceFile are only
   * initialized during the reader phase.
   */
  def lookupSourceFile(filename: String): SourceFile = {
    val file = new File(filename)
    _cache getOrElseUpdate(file.getCanonicalPath, SourceFile(file))
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
  case class SourceFile(file: File) {

    /**
     * The parsed trees.
     *
     * It's multiple ones since depending on the
     * context of include the file can be parsed with different
     * parsers.
     */
    def trees: Seq[ASTNode] = _parsers.values.toSeq

    /**
     * The canonical name of the file
     */
    def filename: String = file.getCanonicalPath

    /**
     * The textual contents of the file
     */
    def contents: BufferedReader = IO.filereader(filename)

    /**
     * Reads the file the given parser
     */
    def readWith(p: TreeParser): ASTNode = 
      _parsers getOrElseUpdate(p, {
        val tree = parsers.parseFile(p, filename, contents)
        initTree(tree)
        tree;
      })

    
    private val _parsers = mutable.Map[TreeParser, ASTNode]()
  }

  private val Dummy = SourceFile(new File("$$DUMMY$$"))

  // map from canonical path to sourcefile
  private var _cache = mutable.HashMap[String, SourceFile]()


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
  private lazy val collectSourceFiles: ASTNode => List[SourceFile] = attr { case node =>

    var dependencies = List.empty[SourceFile]

    // strategy to collect dependencies
    val collect = query {

      case inc @ Include(filename, parser: TreeParser) => {

        val file = new File(filename)
        val path = file.getCanonicalPath

        dependencies = dependencies ++ (_cache get(path) match {

          // Someone else put a `Dummy` into the cache and did not finish
          // computation => cycle
          case Some(`Dummy`) => sys error (s"Cyclic dependency at ${inc}")

          // Already processed, so it's dependencies are already resolved 
          // Still requires processing of the possibly new tree
          // (This is due to the fact, that `SourceFile`s can have
          // multiple trees, depending on the used parser)
          case Some(source) => (source readWith parser)->collectSourceFiles
          
          // Put a dummy if SourceFile hasn't been processed, yet.
          // Then resolve dependencies of `source` before updating
          // the cache
          case None => {
            
            _cache update(path, Dummy)
            val source = SourceFile(file)
            val deps = (source readWith parser)->collectSourceFiles
            _cache update(path, source)

            deps :+ source
          }
        })
      }
    }

    everywheretd (collect) (node)

    dependencies.distinct
  }

}