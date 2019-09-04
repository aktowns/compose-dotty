package tinyfp.typeclass

/** The Functor class is used for types that can be mapped over. */
trait Functor[F[_]]:
  /** Apply a function to values inside the functor */
  def (fa: F[A]) map [A, B] (f: A => B): F[B]

  /** @see Symbol for [[Functor.map]] */
  def (ff: A => B) <#> [A, B] (fa: F[A]): F[B] = fa.map(ff)
  /** Replace all locations in the input with the same value. */
  def (a: A) <# [A, B] (fb: F[B]): F[A] = fb.map(_ => a)

object Functor:
  def apply[T[_]] given Functor[T] = the[Functor[T]]
