package tinyfp.typeclass

trait MonadTrans[F[_[_], _]]:
  def (fa: G[A]) lift [G[_]: Monad, A]: F[G, A]

  implicit def apply[G[_]: Monad]: Monad[[A] =>> F[G, A]]

object MonadTrans:
  def apply[T[_[_], _]] given MonadTrans[T] = the[MonadTrans[T]]


//trait Hoist[F[_[_], _]] extends MonadTrans[F]:
//  def hoist[M[_]: Monad, N[_], A](f: M ~> N): F[M, A] => F[N, A]