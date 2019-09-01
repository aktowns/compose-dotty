package typeclass

/** The class of monoids (types with an associative binary operation that has an identity). */
trait Monoid[T] extends Semigroup[T]:
  /** Identity of mappend */
  def unit: T

object Monoid:
  def apply[T] given Monoid[T] = the[Monoid[T]]
