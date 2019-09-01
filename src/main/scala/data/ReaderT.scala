package data

import typeclass._

case class ReaderT[R, F[_], A](runReaderT: R => F[A])

object ReaderT:
  def ask[R, M[_]] given (m: Monad[M]): ReaderT[R, M, R] = 
    ReaderT(m.pure)

type Reader[R, A] = ReaderT[R, Identity, A]

given ReaderTMonad[R, M[_]] as Monad[[A] =>> ReaderT[R, M, A]] given (mon: Monad[M]):
  def (x: ReaderT[R, M, A]) >>= [A, B] (f: A => ReaderT[R, M, B]): ReaderT[R, M, B] =
    ReaderT(r =>
      mon.flatMap(x.runReaderT(r)) { (a: A) =>
        f(a).runReaderT(r)
      }
    )

  def pure[A](a: A): ReaderT[R, M, A] = ReaderT(r => mon.pure(a))

given ReaderTMonadTrans[R] as MonadTrans[[M[_], A] =>> ReaderT[R, M, A]]: 
  def (fa: G[A]) lift [G[_]: Monad, A]: ReaderT[R, G, A] =
    ReaderT(_ => fa)
  
  def apply[G[_]: Monad]: Monad[[A] =>> ReaderT[R, G, A]] = 
    ReaderTMonad[R, G]
