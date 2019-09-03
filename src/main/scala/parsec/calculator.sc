import typeclass._
import given instance._

import parsec._
import given parsec._

trait Expr
case class Add(e1: Expr, e2: Expr) extends Expr
case class Mul(e1: Expr, e2: Expr) extends Expr
case class Sub(e1: Expr, e2: Expr) extends Expr
case class Lit(l1: Int) extends Expr

def eval(e: Expr): Int =
  e match
    case Add(e1, e2) => eval(e1) + eval(e2)
    case Mul(e1, e2) => eval(e1) * eval(e2)
    case Sub(e1, e2) => eval(e1) + eval(e2)
    case Lit(n)      => n

eval(Add(Lit(1), Lit(41)))

object grmr:
  def int: Parser[Expr] = 
    tap("int", number.flatMap{n => 
      tap("int-inner", n)
      Monad[Parser].pure(Lit(n))
    })
  
  def infixOp[A](x: String)(f: A => A => A): Parser[A => A => A] = 
    reserved(x) >> tap(s"consumed infix: $x", Monad[Parser].pure(f))
  
  def mulop: Parser[Expr => Expr => Expr] = 
    infixOp("*")(Mul.curried)
  
  def addop: Parser[Expr => Expr => Expr] = 
    infixOp("+")(Add.curried) <|> infixOp("-")(Sub.curried)

  def expr: Parser[Expr] = 
    chainl1(term)(addop)
  
  def factor: Parser[Expr] = 
    int <|> parens(expr)
  
  def term: Parser[Expr] = 
    chainl1(factor)(mulop)



def infixOp[A](x: String)(f: A => A => A): Parser[A => A => A] = 
  reserved(x) >> tap(s"consumed infix: $x", Monad[Parser].pure(f))

def mulop: Parser[Expr => Expr => Expr] = 
  infixOp("*")(Mul.curried)

def run(s: String): Expr =
  runParser(chainl1(grmr.int)(mulop), s)

run("*")

//run("1+1")
