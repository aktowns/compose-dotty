/* 
    This worksheet is a toy calculator language using tinyfp.parsec
    it converts simple expressions ie "1+(10*2)" to a syntax tree then
    evaluates the response
*/
import tinyfp.typeclass._
import given tinyfp.instance.{Monad}

import tinyfp.parsec._
import given tinyfp.parsec._

enum Expr:
  case Add(e1: Expr, e2: Expr)
  case Mul(e1: Expr, e2: Expr)
  case Sub(e1: Expr, e2: Expr)
  case Lit(l1: Int)

def eval(e: Expr): Int =
  e match
    case Expr.Add(e1, e2) => eval(e1) + eval(e2)
    case Expr.Mul(e1, e2) => eval(e1) * eval(e2)
    case Expr.Sub(e1, e2) => eval(e1) + eval(e2)
    case Expr.Lit(n)      => n

eval(Expr.Add(Expr.Lit(1), Expr.Lit(41))) // 42

object grmr:
  import Expr._

  def int: Parser[Expr] = 
    number.flatMap{n => Monad[Parser].pure(Lit(n)) }
  
  def infixOp[A](x: String)(f: A => A => A): Parser[A => A => A] = 
    tap(s"infixOp: $x", reserved(x) >> Monad[Parser].pure(f))
  
  def mulop: Parser[Expr => Expr => Expr] = 
    tap("mulop", infixOp("*")(Mul.apply.curried))
  
  def addop: Parser[Expr => Expr => Expr] = 
    tap("addop", infixOp("+")(Add.apply.curried) <|> infixOp("-")(Sub.apply.curried))

  def expr: Parser[Expr] =
    tap("expr", chainl1(term)(addop))
  
  def factor: Parser[Expr] = 
    tap("factor", int <|> parens(expr))
  
  def term: Parser[Expr] = 
    tap("term", chainl1(factor)(mulop))

def run(s: String): Expr =
  runParser(grmr.expr, s)


run("1+1")
