import data._
import typeclass._

case class ParseError()

type Cb[A,S,U,M[_],B] = A => State[S, U] => ParseError => M[B]

//trait Cb0[A,S,U,M[_],Z]:
//  def apply[B](value: Cb[A,S,U,M,B]): Z

case class Cb1[A,S,U,M[_]]():
  def apply[B](value: Cb[A,S,U,M,B]): Cb[A,S,U,M,B]

trait ForAll[F[_]]:
  def apply[A]: F[A]

type Test[A,B] = A => B

case class Test1[A]() extends ForAll[[B] =>> A => B]
  def apply[B](x: A => B) = x


Test1((x: Int) => "Hello")


case class ParsecT[S, U, M[_], A](
  runParsecT: [B] =>> State[S, U] 
                  => (A => State[S, U] => ParseError => M[B]) // consumed ok
                  => (ParseError => M[B])                     // consumed err
                  => (A => State[S, U] => ParseError => M[B]) // empty ok
                  => (ParseError => M[B])                     // empty err
                  => M[B])