package choicecalculus
package lang.choicecalculus

import org.scalatest._
import org.scalatest.matchers.ShouldMatchers._
import org.kiama.util.RegexParserTests
import utility.test

import lang.javascript.implicits._

class ParserTests extends FlatSpec with test.Helpers {

  import lang.javascript.{ BlockStmt, CallExpr, FunctionDecl, GroupExpr, Program, 
                           NameAccessExpr, SequenceExpr, ReturnStmt, VarDeclStmt,
                           VarBinding, EmptyStmt }
  
  object parser extends ChoiceCalculusParser with RegexParserTests {
    
    ignore("parse choice expressions with commas inside") {
      
      val expected = choice('A) ( 
        'a -> lit("10"),
        'b -> lit("15")
      )
      
      assertParseOk("A<a: 10, b: 15>", expression, expected)
      assertParseOk("A<a: 10, b: 15>", statement, expected)
      
    }
    
    ignore("parse choice expressions as operands") {
      
      val expected = choice('A) (
        'a -> lit("10"),
        'b -> lit("15")
      ) + choice('A) ( 
        'a -> lit("10"),
        'b -> lit("15")
      )
      
      assertParseOk("A<a: 10, b: 15> + A<a: 10, b: 15>", expression, expected)
    }
    
    it should "not parse identifiers as keywords" in {
      
      assertParseOk("selector.charAt(0)", expression, 
          CallExpr(NameAccessExpr("selector", "charAt"), List(lit("0"))))
      
      assertParseOk("dimension(a) +  4", expression, 
          CallExpr("dimension",List(lit("a"))) + lit("4"))
      
    }
    
    it should "parse choice calculus expressions without parenthesis" in {
      
      assertParseOk("3 + dim A(a) in (4 + 4)", expression, 
          lit("3") + dim('A)('a) { GroupExpr(lit("4") + lit("4")) })
      
      val expected = lit("3") + select('A, 'a, dim('A)('a) { choice('A) ('a -> (lit("4") + lit("5"))) })
      
      assertParseOk("3 + select A.a from dim A(a) in choice A {\n  case a → 4 + 5\n}", expression, expected)
          
    }
    
    it should "parse share expressions" in {
      assertParseOk("share #x:Expression as 4 within #x", expression, share('x, lit("4"), id('x)))
    }
    
    it should "parse statements as declaration bodies" in {
      
      assertParseOk("dim A(a) { function Foo() { return 3 } }", statement,
          dim('A)('a) { BlockStmt(List(
            FunctionDecl(lit("Foo"),List(),BlockStmt(List(ReturnStmt(Some(lit("3"))))))))
          })
      
    }
    
    it should "parse statements as select bodies" in {
      
      val toParse = """select A.a from {
        dim A(a) { 3 + 4 }
      }"""
      
      val expected = select('A, 'a, BlockStmt(List(
        dim('A)('a) { BlockStmt(List(
          lit("3") + lit("4")))
        })))
      
       assertParseOk(toParse, statement, expected)
       assertParseOk(toParse, topLevel, Program(List(expected)))
       assertParseOk(toParse, parser, Program(List(expected)))
    }
    
    it should "parse sequence expressions with correct precedence" in {
      
      assertParseOk("dim A(a) 4, dim B(b) 5", expression, 
        dim('A)('a) { SequenceExpr(List(lit("4"), 
          dim('B)('b) { lit("5") })) 
        })
     
      assertParseOk("(dim A(a) 4), dim B(b) 5", expression, 
          SequenceExpr(List(GroupExpr(
            dim('A)('a) { lit("4") }), 
              dim('B)('b) { lit("5") })))
    }
    
    it should "ignore leading and trailing whitespacess when using strippedPhrase" in {
      
      val expected = dim('A)('a) { lit("4") }
      
      assertParseOk("     dim A(a) 4", strippedPhrase(expression), expected)
      assertParseOk("dim A(a) 4     ", strippedPhrase(expression), expected)
      assertParseOk("     dim A(a) 4   ", strippedPhrase(expression), expected)
    }

    it should "parse choicecalculus terms in expression position" in {

      locally {
        val expected = Program(List(
          dim('A)('a, 'b) {
            FunctionDecl(lit("Foo"),List(),BlockStmt(List(
              ReturnStmt(Some(
                choice('A)(
                  'a -> lit("3"),
                  'b -> lit("4")))))))
          }))

        assertParseOk("""
          dim A(a,b) in
          function Foo() {
            return choice A { 
              case a => 3
              case b => 4 
            }
          }
        """, topLevel, expected)
      }

      assertParseOk("#x + #x", expression, id('x) + id('x))
      assertParseOk("#y + #y", assignExpr, id('y) + id('y))
      assertParseOk("(#z + #z)", primExpr, GroupExpr(id('z) + id('z)))

      assertParseOk("#x, #x", expression, SequenceExpr(id('x) :: id('x) :: Nil))
      assertParseOk("var x = #x", declaration, 
        VarDeclStmt(VarBinding(lit("x"),id('x)) :: Nil))
    }

    it should "parse choicecalculus terms in statement position" in {

      val sc = EmptyStmt

      assertParseOk("""#x""", declaration, id('x))
      assertParseOk("""#y""", statement, id('y))
      assertParseOk("""{#x;#y;#z}""", statement, 
        BlockStmt(List(id('x), sc, id('y), sc, id('z))))

      assertParseOk("""dim A(a,b) {
        var x = 42
      }""", declaration, dim('A)('a,'b) { 
        BlockStmt(VarDeclStmt(VarBinding(lit("x"),lit("42")) :: Nil) :: Nil)
      })
    }

    it should "parse mixed nested choicecalculus terms and javascript terms" in {

      assertParseOk("""dim A(a,b) {
        var x = #z
      }""", declaration, dim('A)('a,'b) { 
        BlockStmt(VarDeclStmt(VarBinding(lit("x"),id('z)) :: Nil) :: Nil)
      })

      assertParseOk("""function Foo() {
        dim A(a,b) {
          var x = #z
        }
      }""", declaration, FunctionDecl(lit("Foo"),List(),BlockStmt(List(
        dim('A)('a,'b) {
          BlockStmt(VarDeclStmt(
            VarBinding(lit("x"), id('z)) :: Nil) :: Nil)
        }
      ))))

    }

    it should "parse multiple nested dimension declarations" in {
        
      val expected = dim('A)('a,'b) { 
        dim('B)('a,'b) {
          dim('C)('a,'b) {
            GroupExpr(choice('A)(
              'a -> lit("1"),
              'b -> lit("2")
            ) + choice('B)(
              'a -> lit("3"),
              'b -> lit("4")
            ))
          }
        }
      }

      assertParseOk("""
         dim A(a, b) in
         dim B(a, b) in
         dim C(a, b) in
           (choice A {
             case a => 1
             case b => 2
           } + choice B {
             case a => 3
             case b => 4
           })""", strippedPhrase(expression), expected)
    }
    
    // TODO implement after adding sensible error handling
    ignore("print pretty error messages") {

      // should yield "[1.18] failure: `(' expected but `*' found"
      assertParseOk("""dim A(a,b) in (3+*4)""", topLevel, Program(Nil))

    }
  }
  
  parser
}