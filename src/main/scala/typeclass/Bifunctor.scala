package typeclass

trait Bifunctor[F[_, _]]:
  def (fab: F[A, B]) bimap  [A, B, C, D] (fa: A => C) (fb: B => D): F[C, D]
  def (fab: F[A, B]) first  [A, B, C]    (fa: A => C): F[C, B] = fab.bimap(fa)(identity)
  def (fab: F[A, B]) second [A, B, C]    (fa: B => C): F[A, C] = fab.bimap(identity)(fa)

object Bifunctor:
  def apply[T[_, _]] given Bifunctor[T] = the[Bifunctor[T]]

