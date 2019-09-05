package tinyfp.instance

import tinyfp.typeclass._

given EitherRightBiasedFunctor[L] as Functor[[R] =>> Either[L, R]]:
  def (fa: Either[L, A]) map [A, B] (f: A => B): Either[L, B] =
    fa match
      case Right(x) => Right(f(x))
      case Left(x) => Left(x)

given EitherRightBiasedApplicative[L] as Applicative[[R] =>> Either[L, R]]:
  def (f: => Either[L, A => B]) <*> [A, B] (fa: => Either[L, A]): Either[L, B] =
    f match
      case Right(ff) => fa.map(ff)
      case Left(x) => Left(x)

  def pure[A](a: => A): Either[L, A] = Right(a)

given EitherRightBiasedMonad[L] as Monad[[R] =>> Either[L, R]]:
  def (fa: Either[L, A]) >>= [A, B] (f: A => Either[L, B]): Either[L, B] =
    fa match
      case Right(a) => f(a)
      case Left(x) => Left(x)

  def pure[A](a: => A): Either[L, A] = Right(a)

given EitherBifunctor as Bifunctor[Either]:
  def (fab: Either[A, B]) bimap [A, B, C, D] (fa: A => C) (fb: B => D): Either[C, D] =
    fab match
      case Left(l)  => Left(fa(l))
      case Right(r) => Right(fb(r))