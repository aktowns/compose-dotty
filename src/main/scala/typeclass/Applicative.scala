package typeclass

/** A functor with application, providing operations to
 *
 *  - embed pure expressions (pure), and
 *  - sequence computations and combine their results (<*> and liftA2).
 */
trait Applicative[F[_]] extends Functor[F]:
  /** Sequential application. */
  def (f: F[A => B]) <*> [A, B] (fa: F[A]): F[B]
  /** Lift a value. */
  def pure[A](a: A): F[A]

  /** @see [[Functor.map]] */
  override def (fa: F[A]) map [A, B] (f: A => B): F[B]                = pure(f) <*> fa

  def (f: F[A => B]) ap [A, B] (fa: F[A]): F[B] = f <*> fa

  /** Lift a binary function to actions. */
  def (f: A => (B => C)) liftA2 [A, B, C] (fa: F[A]) (fb: F[B]): F[C] = f <#> fa <*> fb
  /** Sequence actions, discarding the value of the first argument. */
  def (fa: F[A]) *> [A, B] (fb: F[B]): F[B]                           = (identity[B] <# fa) <*> fb
  /** Sequence actions, discarding the value of the second argument. */
  def (fa: F[A]) <* [A, B] (fb: F[B]): F[A]                           = liftA2((x: A) => (_: B) => x)(fa)(fb)
  /** A variant of <*> with the arguments reversed. */
  def (fa: F[A]) <**> [A, B] (f: F[A => B]): F[B]                     = f <*> fa

object Applicative:
  def apply[T[_]] given Applicative[T] = the[Applicative[T]]
