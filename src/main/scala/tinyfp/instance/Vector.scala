package tinyfp.instance

import tinyfp.typeclass._

given VectorFoldable as Foldable[Vector]:
  def (t: Vector[A]) foldr [A, B] (z: B)(f: A => B => B): B = 
    t.foldRight(z)(Function.uncurried(f))

given VectorMonoid[A] as Monoid[Vector[A]]:
  def (x: Vector[A]) <> (y: Vector[A]): Vector[A] = x ++ y

  def mempty: Vector[A] = Vector()

given VectorFunctor as Functor[Vector]:
  def (fa: Vector[A]) map [A, B] (f: A => B): Vector[B] = fa.map(f)

given VectorApplicative as Applicative[Vector] = VectorAlternative

given VectorAlternative as Alternative[Vector]:
  def (fa: => Vector[A]) <|> [A] (fb: => Vector[A]) = fa ++ fb
  def empty[A] = Vector[A]()

  // Applicative
  def (f: => Vector[A => B]) <*> [A, B] (fa: => Vector[A]): Vector[B] = 
    fa.map(x => f.map(y => y(x))).flatten
  def pure[A](a: => A): Vector[A] = Vector(a)
