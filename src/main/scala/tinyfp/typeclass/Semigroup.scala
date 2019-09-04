package tinyfp.typeclass

/** The class of semigroups (types with an associative binary operation). */
trait Semigroup[T]:
  /** An associative operation. */
  def (x: T) <> (y: T): T

  def (x: T) mappend (y: T): T = x <> y

object Semigroup:
  def apply[T] given Semigroup[T] = the[Semigroup[T]]
