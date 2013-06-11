package choicecalculus
package semantics

import ast.{ ASTNode, Program, IncludeExpr }
import scala.collection.mutable
import org.kiama.util.{ Compiler, PositionedParserUtilities }
import org.kiama.util.IO.{filereader, FileNotFoundException }

trait Includes {
  self: PositionedParserUtilities with Compiler[ASTNode] with Semantics =>
  
  case class SourceFile(path: String, ast: ASTNode, dimensions: Option[DimensionGraph]) {
    override def toString = "SourceFile(%s, ..., %s)".format(path, dimensions)
  }
  case object Dummy extends SourceFile("", Program(List()), None)
  
  val files: mutable.HashMap[String, SourceFile] = mutable.HashMap()
    
  
  def cached(filename: String)(action: => Either[String, SourceFile]): Unit = files.getOrElse(filename, None) match {
    case Dummy => sys error "Cycle in file dependencies!"
    case s@SourceFile(path, ast, Some(dims)) => s
    case None => {
      files.update(filename, Dummy)
      action match {
        case Left(msg) => println(msg)
        case Right(file) => files.update(filename, file)
      }
    }
  }
  
  def processFileWithParser(filename: String, parser: Parser[ASTNode]): Unit = cached(filename) {    
      
    try {      
      val reader = filereader (filename, encoding)
      
      println("Trying to parse "+ filename + " with parser: " + parser.toString)
      
      parseAll(phrase(memo(parser)), reader) match {
        case Success(ast, _) => processTree(ast) match { 
          case reduced => Right(SourceFile(filename, reduced, Some(dimensioning(reduced))))
        }
        case f => Left(f.toString)
        }
    } catch {
      case e: FileNotFoundException => Left(e.message)      
    }
  }
    
  def fileDimensions(include: IncludeExpr[_,_]): DimensionGraph = include match {
    case IncludeExpr(filename, p:Parser[ASTNode]) => {
      processFileWithParser(filename, p)
      println(files.get(filename))
      val Some(SourceFile(_, _, Some(dim))) = files.get(filename)
      dim
    }
  }
  
  def fileContents(include: IncludeExpr[_,_]): ASTNode = include match {
    case IncludeExpr(filename, p:Parser[ASTNode]) => {
      processFileWithParser(filename, p)
      val Some(SourceFile(_, ast, _)) = files.get(filename)
      ast
    }
  }
}