package instance

import typeclass._

//given ListMonoid[A] as Monoid[List[A]]:
//  def (x: List[A]) <> (y: List[A]): List[A] = x ++ y
//
//  def unit: List[A] = List()

given ListFunctor as Functor[List]:
  def (fa: List[A]) map [A, B] (f: A => B): List[B] = fa.map(f)

given ListApplicative as Applicative[List] = ListAlternative

given ListAlternative as Alternative[List]:
  def (fa: List[A]) <|> [A] (fb: List[A]) = fa ++ fb
  def empty[A] = List[A]()

  // Applicative
  def (f: List[A => B]) <*> [A, B] (fa: List[A]): List[B] = fa.map(x => f.map(y => y(x))).flatten
  def pure[A](a: A): List[A] = List(a)