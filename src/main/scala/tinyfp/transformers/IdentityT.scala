package tinyfp.transformers

import tinyfp.typeclass._

/** The identity monad transformer.

This is useful for functions parameterized by a monad transformer.
*/
case class IdentityT[F[_], A](runIdentityT: F[A])

given IdentityTFunctor[M[_]] as Functor[[A] =>> IdentityT[M, A]] given (fun: Functor[M]):
  def (fa: IdentityT[M, A]) map [A, B] (f: A => B): IdentityT[M, B] =
    IdentityT(fa.runIdentityT.map(f))

given IdentityTApplicative[M[_]] as Applicative[[A] =>> IdentityT[M, A]] given (app: Applicative[M]):
  def (f: IdentityT[M, A => B]) <*> [A, B] (fa: IdentityT[M, A]): IdentityT[M, B] =
    IdentityT(app.ap(f.runIdentityT)(fa.runIdentityT))

  def pure[A](a: A): IdentityT[M, A] = IdentityT(app.pure(a))

given IdentityTMonad[M[_]] as Monad[[A] =>> IdentityT[M, A]] given (mon: Monad[M]):
  def (x: IdentityT[M, A]) >>= [A, B] (f: A => IdentityT[M, B]): IdentityT[M, B] =
    IdentityT(mon.flatMap(x.runIdentityT)(f.andThen(_.runIdentityT)))

  def pure[A](a: A): IdentityT[M, A] = IdentityT(mon.pure(a))

// given IdentityTMonadTrans as MonadTrans[IdentityT]:
//   def (fa: G[A]) lift [G[_]: Monad, A]: IdentityT[G, A] = IdentityT(fa)
// 
//   def apply[G[_]: Monad]: Monad[[A] =>> IdentityT[G, A]] = 
//     IdentityTMonad[G]