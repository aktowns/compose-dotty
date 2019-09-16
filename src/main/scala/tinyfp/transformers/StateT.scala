package tinyfp.transformers

import tinyfp.typeclass._
import tinyfp.data.Identity
import given tinyfp.data._

case class StateT[S, F[_], A](runStateT: S => F[(A, S)])

type State[S, A] = StateT[S, Identity, A]

object StateT:
  def get[S, M[_]] given (m: Monad[M]): StateT[S, M, S] = 
    StateT((s: S) => m.pure((s, s)))

  def put[S, M[_]](s: S) given (m: Monad[M]): StateT[S, M, Unit] =
    StateT((_: S) => m.pure((), s))  

  def modify[S, M[_]](f: S => S) given (m: Monad[M]): StateT[S, M, Unit] =
    StateT((s: S) => m.pure((), f(s)))

  def gets[S, A, M[_]](f: S => A) given (m: Monad[M]): StateT[S, M, A] = 
    StateT((s: S) => m.pure(f(s), s))

  def (fa: G[A]) lift [R, G[_]: Monad, A]: StateT[R, G, A] =
    StateTMonadTrans[R].lift(fa)

given StateTMonad[S, M[_]] as Monad[[A] =>> StateT[S, M, A]] given (mon: Monad[M]):
  def (x: StateT[S, M, A]) >>= [A, B] (f: A => StateT[S, M, B]): StateT[S, M, B] =
    StateT(s =>
      mon.flatMap(x.runStateT(s)) { (a: A, ss: S) =>
        f(a).runStateT(ss)
      }
    )

  def pure[A](a: => A): StateT[S, M, A] = StateT(s => mon.pure((a, s)))

given StateTMonadTrans[S] as MonadTrans[[M[_], A] =>> StateT[S, M, A]]: 
  def (fa: G[A]) lift [G[_], A] given (m: Monad[G]): StateT[S, G, A] =
    StateT(s => fa.flatMap(a => m.pure((a, s))))
  
  def apply[G[_]: Monad]: Monad[[A] =>> StateT[S, G, A]] = 
    StateTMonad[S, G]
