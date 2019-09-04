package tinyfp.parsec

import tinyfp.data._
import tinyfp.typeclass._
import given tinyfp.instance._

case class SourcePos()
case class ParseError()

case class Parser[A](parse: Seq[Char] => List[(A, Seq[Char])])

def tap[A](m: String, v: => A): A =
  println(s"$m: $v")
  Console.flush()
  v

given ParserFunctor as Functor[Parser]:
  def (fa: Parser[A]) map [A, B] (f: A => B): Parser[B] = 
    Parser((s: Seq[Char]) => fa.parse(s).map((a, b) => (f(a), b)))

given ParserApplicative as Applicative[Parser]:
  def (fa: Parser[A => B]) <*> [A, B] (fb: Parser[A]): Parser[B] =
    Parser((s: Seq[Char]) => fa.parse(s).flatMap { (f: A => B, s1: Seq[Char]) =>
      fb.parse(s1).map((a: A, s2: Seq[Char]) => (f(a), s2))
    })
  def pure[A](v: A): Parser[A] = Parser((s) => List((v, s)))

given ParserMonad as Monad[Parser] given (f: Applicative[Parser]):
  def (fa: Parser[A]) >>= [A, B] (f: A => Parser[B]): Parser[B] =
    Parser(s => fa.parse(s).concatMap((a: A, ss: Seq[Char]) => f(a).parse(ss)))

  def pure[A](v: A): Parser[A] = f.pure(v)

given ParserAlternative as Alternative[Parser] given (f: Applicative[Parser]):
  def (fa: => Parser[A]) <|> [A] (fb: => Parser[A]): Parser[A] = 
    Parser { s =>  
      fa.parse(s) match
        case List() => tap("<|> right", fb.parse(s))
        case res => tap("left <|>", res)
    }
  def empty[A]: Parser[A] = failure

  def (fa: Parser[A => B]) <*> [A, B] (fb: Parser[A]): Parser[B] = f.ap(fa)(fb)
  def pure[A](v: A): Parser[A] = f.pure(v)


def runParser[A](m: Parser[A], s: String): A =
  m.parse(s) match
    case List((res, Nil)) => res
    case List((_, rs))    => throw (new RuntimeException("did not consume entire stream"))
    case x                => throw (new RuntimeException(s"parse error: $x"))

def item: Parser[Char] = Parser { s => 
  s match 
    case Nil                 => Nil
    case Seq(h: Char, t: _*) => tap("item", List(Tuple2(h, t)))
}

def combine[A](p: Parser[A], q: Parser[A]): Parser[A] = 
  Parser(s => p.parse(s) ++ q.parse(s))

def failure[A]: Parser[A] = Parser(_ => List())

def satisfy(p: Char => Boolean): Parser[Char] = item >>= { (c: Char) => 
  if (p(c)) { Monad[Parser].pure(c) } 
  else { Parser(cs => List()) }
}

def oneOf(s: String): Parser[Char] = 
  satisfy(c => s.contains(c))

def chainl1[A](p: Parser[A])(op: Parser[A => A => A]): Parser[A] =
  def rest(a: A): Parser[A] =
    for
      f <- op
      b <- p
      r <- tap("chain-rest", rest(f(a)(b))) <|> tap("chain-left", Monad[Parser].pure(a))
    yield r

  p.flatMap(rest)

def chainl[A](p: Parser[A])(op: Parser[A => A => A])(a: A): Parser[A] =
  chainl1(p)(op) <|> Monad[Parser].pure(a)

def char(c: Char): Parser[Char] = satisfy(cc => cc == c)

def nat: Parser[Int] = 
  satisfy(c => c.isDigit).some.map(_.mkString.toInt)

def str(s: String): Parser[String] = 
  if (s.isEmpty) then Monad[Parser].pure("")
  else
    for
      _ <- char(s.head)
      _ <- str(s.tail) 
    yield tap("str", s)

def token[A](p: Parser[A]): Parser[A] =
  p.flatMap(a => spaces >> Monad[Parser].pure(a))

def reserved(s: String): Parser[String] =
  token(str(s))

def spaces: Parser[String] = oneOf(" \n\r").many.map(_.mkString)

def digits: Parser[Char] = satisfy(_.isDigit)

def number: Parser[Int] =
  for
    s <- satisfy(_.equals('-')).map(_ => true) <|> Monad[Parser].pure(false)
    cs <- digits.some.map(_.mkString.toInt)
  yield if (s) then cs.unary_- else cs
    
def parens[A](m: Parser[A]): Parser[A] = 
  for
    _ <- reserved("(")
    n <- m
    _ <- reserved(")")
  yield n