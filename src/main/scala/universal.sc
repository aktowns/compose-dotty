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

trait Case5[F, In1, In2, In3, In4, In5]:
  type Out
  def apply(a: In1, b: In2, c: In3, d: In4, e: In5): Out

trait Poly5:
  def apply[A,B,C,D,E](a: A, b: B, c: C, d: D, e: E)
                        (implicit cs: Case5[this.type, A, B, C, D, E]): cs.Out = 
  cs(a,b,c,d,e)

object runParsecT extends Poly5:
  implicit def runParse[B] = new Case5[runParsecT.type, 
                                       State[S, U], 
                                       Function3[A, State[S, U], ParseError, M[B]],
                                       Function1[ParseError, M[B],
                                       Function3[A, State[S, U], ParseError, M[B]],
                                       Function1[ParseError, M[B]]] {
    type Out = M[B]
    def apply(a: State[S, U], 
              b: A => State[S, U] => ParseError => M[Out], 
              c: ParseError => M[Out], 
              d: A => State[S, U] => ParseError => M[Out], 
              e: ParseError => M[Out] 
              ): Out = ???
  }

//case class ParsecT[S, U, M[_], A](runParsecT: RunParsecT[S, U, M, A])
  
  
//  [B] =>> State[S, U] 
//                  => (A => State[S, U] => ParseError => M[B]) // consumed ok
//                  => (ParseError => M[B])                     // consumed err
//                  => (A => State[S, U] => ParseError => M[B]) // empty ok
//                  => (ParseError => M[B])                     // empty err
//                  => M[B])
//