package choicecalculus
package ast

import org.kiama.output.ParenPrettyPrinter

trait JavaScriptPP extends PrettyPrinter  {
  
  override val defaultIndent = 2
  override val defaultWidth = 80
  
  def toDocOrEmpty(e: Option[ASTNode]) = e match {
    case Some(e) => toDoc(e)
    case None => empty
  }
  
  def stmtsep(stmts: List[Statement]) = stmts match {
    case Nil => empty
    case stmt :: Nil => toDoc(stmt)
    case many => many.init.foldRight( toDoc(many.last) ) {
      case (e:Expression, rest) => toDoc(e) <> semi <> linebreak <> rest
      case (s, rest) => toDoc(s) <> linebreak <> rest
    }
  }
  
  def nestIfNoBlock(e: ASTNode) = e match {
    case b:BlockStmt => toDoc(b)
    case other => nest( line <> toDoc(other) ) <> linebreak
  }
  
  override def toDoc(e: ASTNode): Doc = e match {

    case Literal(contents) => 
      text(contents) 
    
    case Program(contents) => 
      stmtsep(contents)

    case VarDeclStmt(bindings) => 
      "var" <+> nest ( fillsep (bindings.map(toDoc), comma))
    
    case VarBinding(name, binding) => 
      toDoc(name) <+> equal <+> toDoc(binding)
      
    case BlockStmt(stmts) => 
      braces( nest( line <> stmtsep(stmts)) <> line)
    
    case IfStmt(cond, thenBlock, elseBlock) => 
      "if" <> parens(toDoc(cond)) <+> nestIfNoBlock(thenBlock) <> (elseBlock match {
        
        // this space should be a "collapsing space"
        case Some(e) => space <> "else" <+> nestIfNoBlock(e)
        case _ => empty
      })
      
    case WhileStmt(cond, body) => 
      "while" <> parens(toDoc(cond)) <+> 
        nestIfNoBlock(body)
    
    case DoWhileStmt(body, cond) => 
      "do" <+> nestIfNoBlock(body) <+> 
      "while" <> parens(toDoc(cond))
    
    case ForStmt(init, cond, incr, body) => 
      "for" <> parens(toDocOrEmpty(init) <+> semi <+> toDocOrEmpty(cond) <+> semi <+> toDocOrEmpty(incr)) <+> 
        nestIfNoBlock(body)
      
    case ForInStmt(init, collection, body) => 
      "for" <> parens(toDoc(init) <+> "in" <+> toDoc(collection)) <+> toDoc(body)
      
    case SwitchStmt(head, cases) => 
      "switch" <> parens(toDoc(head)) <+> braces(nest (line <> lsep (cases.map(toDoc), line) <> line))

    case MatchingCase(matcher, stmts) => 
      "case" <+> toDoc(matcher) <> ":" <> nest(line <> lsep (stmts.map(toDoc), line))
      
    case DefaultCase(stmts) => 
      "default" <> ":" <> nest(line <> lsep (stmts.map(toDoc), line))
      
    case BreakStmt(label) => 
      "break" <+> toDocOrEmpty(label)
      
    case ContinueStmt(label) => 
      "continue" <+> toDocOrEmpty(label)
      
    case ThrowStmt(body) => 
      "throw" <+> toDoc(body)

    case TryStmt(body, catchBlock, finallyBlock) => 
      "try" <+> toDoc(body) <+> toDocOrEmpty(catchBlock) <+> toDocOrEmpty(finallyBlock)
      
    case CatchBlock(name, body) => 
      "catch" <> parens( toDoc(name) ) <+> toDoc(body)
      
    case FinallyBlock(body) => 
      "finally" <+> toDoc(body)

    case ReturnStmt(body) => 
      "return" <+> toDocOrEmpty(body)
    
    case WithStmt(binding, body) => 
      "with" <> parens(toDoc(binding)) <+> nestIfNoBlock(body)
      
    case LabeledStmt(label, body) => 
      toDoc(label) <> ":" <+> toDoc(body)
     
    case EmptyStmt => semi

    case BinaryOpExpr(lhs, op, rhs) => 
      toDoc(lhs) <+> text(op) <+> toDoc(rhs)
    
    case TernaryExpr(cond, trueBlock, falseBlock) => 
      toDoc(cond) <+> question <+> toDoc(trueBlock) <+> colon <+> toDoc(falseBlock)
      
    case PrefixExpr(op, body) if op == "void" | op == "delete" | op == "typeof" =>      
      text(op) <+> toDoc(body)
    
    case PrefixExpr(op, body) =>
      text(op) <> toDoc(body)
      
    case PostfixExpr(body, op) => 
      toDoc(body) <> text(op)
      
    case NewExpr(body) =>
      "new" <+> toDoc(body)
      
    case CallExpr(body, args) =>
      toDoc(body) <> parens( ssep( args.map(toDoc), comma) )
      
    case MemberExpr(body, access) => 
      toDoc(body) <> "[" <> toDoc(access) <> "]"
      
    case NameAccessExpr(body, name) =>
      toDoc(body) <> dot <> toDoc(name)
      
    case ArrayExpr(contents) =>
      brackets ( ssep(contents.map(toDoc), comma) )      
      
    case GroupExpr(content) => 
      parens( toDoc(content) )
    
    case SequenceExpr(contents) =>
      ssep ( contents.map(toDoc), comma )
      
    case ObjectExpr(Nil) => braces(empty)
      
    case ObjectExpr(bindings) => 
      braces( nest( line <> ssep(bindings.map(toDoc), comma <> line))  <> line )
    
      
    case PropertyBinding(name, value) =>
      toDoc(name) <> colon <+> toDoc(value)
      
    case FunctionDecl(name, args, body) =>
      "function" <+> toDoc(name) <> parens( ssep(args.map(toDoc), comma) ) <+> toDoc(body)
      
    case FunctionExpr(args, body) => 
      "function" <> parens( ssep(args.map(toDoc), comma) ) <+> toDoc(body)
    
  }
  
}