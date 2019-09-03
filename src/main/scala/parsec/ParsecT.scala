package parsec

import data._
import typeclass._

case class SourcePos()
case class ParseError()

case class Parser[A](parse: Seq[Char] => List[(A, Seq[Char])])

given ParserFunctor as Functor[Parser]:
  def (fa: Parser[A]) map [A, B] (f: A => B): Parser[B] = 
    Parser((s: Seq[Char]) => fa.parse(s).map((a, b) => (f(a), b)))

given ParserApplicative as Applicative[Parser]:
  def (fa: Parser[A => B]) <*> [A, B] (fb: Parser[A]): Parser[B] =
    Parser((s: Seq[Char]) => fa.parse(s).flatMap { (f: A => B, s1: Seq[Char]) =>
      fb.parse(s1).map((a: A, s2: Seq[Char]) => (f(a), s2))
    })
  def pure[A](v: A): Parser[A] = Parser((s) => List((v, s)))

//given ParserApplicative as Applicative[Parser]:


def runParser[A](m: Parser[A], s: String): A =
  m.parse(s) match
    case List((res, Nil)) => res
    case List((_, rs))    => throw (new RuntimeException("did not consume entire stream"))
    case _                => throw (new RuntimeException("parse error"))

def item(): Parser[Char] = Parser { s => 
  s match 
    case Nil                      => Nil
    case Seq(h: Char, t: _*) => List(Tuple2(h, t))
}

def satisfy(p: Char => Boolean): Parser[Char] = ???
