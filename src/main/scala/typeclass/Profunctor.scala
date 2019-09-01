package typeclass

trait Profunctor[F[_, _]]:
  def (fab: F[A, B]) dimap [A, B, C, D] (fa: C => A) (fb: B => D): F[C, D]
  def (fab: F[A, B]) lmap  [A, B, C]    (fa: C => A): F[C, B] = fab.dimap(fa)(identity)
  def (fab: F[A, B]) rmap  [A, B, C]    (fb: B => C): F[A, C] = fab.dimap[A, B, A, C](identity)(fb)
