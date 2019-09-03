package typeclass

/** Data structures that can be folded. */
trait Foldable[T[_]]:
  /** Right-associative fold of a structure. */
  def (t: T[A]) foldr [A, B] (z: B)(f: A => B => B): B

  /** Map each element of the structure to a monoid, and combine the results. */
  def (t: T[A]) foldMap [A, M] (f: A => M) given (m: Monoid[M]): M =
    foldr(t)(m.mempty)(f.andThen(m.mappend))

  /** Combine the elements of a structure using a monoid. */
  def (t: T[M]) foldM [M: Monoid]: M = foldMap(t)(identity)

  /** Map a function over all the elements of a container and concatenate 
    the resulting lists. */
  def (t: T[A]) concatMap [A, B] (f: A => List[B]): List[B] = 
    foldr(t)(List())((a) => (b) => f(a) ++ b)

object Foldable:
  def apply[T[_]] given Foldable[T] = the[Foldable[T]]