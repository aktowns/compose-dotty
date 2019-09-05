package tinyfp.typeclass

trait Monad[F[_]] extends Applicative[F]:
  /** Sequentially compose two actions, passing any value produced by the first as an 
      argument to the second. */
  def (x: F[A]) >>= [A, B] (f: A => F[B]): F[B]

  def (x: F[A]) flatMap [A, B] (f: A => F[B]): F[B]             = x >>= f

  override def (fa: F[A]) map [A, B] (f: A => B)                = fa >>= (a => pure(f(a)))
  override def (f: => F[A => B]) <*> [A, B] (fa: => F[A]): F[B] = f >>= map(fa)

  /** Same as >>=, but with the arguments interchanged. */
  def (f: A => F[B]) =<< [A, B] (x: F[A])                 = x >>= f
  /** Sequentially compose two actions, discarding any value produced by the first */
  def (fa: => F[A]) >> [A, B] (fb: => F[B]): F[B]               = fa >>= (_ => fb)

object Monad:
  def apply[T[_]] given Monad[T] = the[Monad[T]]