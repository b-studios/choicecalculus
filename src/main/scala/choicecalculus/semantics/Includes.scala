package choicecalculus
package semantics

import lang.ASTNode
import lang.choicecalculus.Include
import lang.javascript.Program
import dimensioning.DimensionGraph
import utility.ParserUtils
import scala.collection.mutable
import org.kiama.util.{ Compiler, PositionedParserUtilities }
import org.kiama.util.IO.{ filereader, FileNotFoundException }
import java.io.File

trait Includes {
  self: ParserUtils with Compiler[ASTNode] with Semantics =>
  
  abstract class Cacheable {
    val path: String
    val ast: ASTNode
    val dimensions: Option[DimensionGraph]
  }
    
  case class SourceFile(path: String, ast: ASTNode, dimensions: Option[DimensionGraph]) extends Cacheable {
    override def toString = s"SourceFile($path, ..., $dimensions)"
  }
  case object Dummy extends Cacheable {
    val path = ""
    val ast = Program(List())
    val dimensions = None
  }
  
  val files: mutable.HashMap[String, Cacheable] = mutable.HashMap()
    
  def cached(filename: String)(action: => Either[String, Cacheable]): Unit = files.getOrElse(filename, None) match {
    case Dummy => sys error "Cycle in file dependencies!"
    case s@SourceFile(path, ast, Some(dims)) => s
    case None => {
      files.update(filename, Dummy)
      action.fold(println, files.update(filename, _))
    }
  }
  
  def processFileWithParser(filename: String, parser: Parser[ASTNode]): Unit = cached(filename) {    
      
    try {      
      val reader = filereader (filename, encoding)
      
      println(s"Trying to parse $filename with parser: ${parser.toString}")
      
      parseAll(strippedPhrase(parser), reader) match {
        case Success(ast, _) => processTree(ast).fold(Left(_), (reduced) =>
          Right(SourceFile(filename, reduced, Some(dimensioning(reduced)))))
        case f => Left(f.toString)
        }
    } catch {
      case e: FileNotFoundException => {
        Left(s"${e.message} absolute path: ${new File(filename).getAbsolutePath()}")       
      }
    }
  }
    
  def fileDimensions(include: Include[_ <: ASTNode,_]): DimensionGraph = include match {
    case Include(filename, p:Parser[ASTNode]) => {
      processFileWithParser(filename, p)
      println(files.get(filename))
      files.get(filename) match {
        case Some(SourceFile(_, _, Some(dim))) => dim
        case _ => sys error s"Error while processing file ${filename}"
      }
    }
  }
  
  def fileContents(include: Include[_ <: ASTNode,_]): ASTNode = include match {
    case Include(filename, p:Parser[ASTNode]) => {
      processFileWithParser(filename, p)
      files.get(filename) match {
        case Some(SourceFile(_, ast, _)) => ast
        case _ => sys error s"Error while processing file ${filename}"
      }
    }
  }
}