package tinyfp.instance

import tinyfp.typeclass._

trait OptionInstances extends Alternative[Option] with Monad[Option]
given as OptionInstances:

  // Functor
  override def (fa: Option[A]) map [A, B] (f: A => B): Option[B] = 
    fa.fold(None)(x => Some(f(x))) 

  // Alternative
  def (fa: => Option[A]) <|> [A] (fb: => Option[A]) =
    (fa, fb) match
      case (None, r) => r
      case (l, _) => l

  def empty[A] = None

  // Applicative
  override def (f: => Option[A => B]) <*> [A, B] (fa: => Option[A]): Option[B] = 
    (f, fa) match
      case (Some(ff), ffa) => ff <#> ffa
      case (None, _)       => None

  override def pure[A](a: => A): Option[A] = Some(a)

  // Monad
  def (fa: Option[A]) >>= [A, B] (f: A => Option[B]): Option[B] =
    fa match
      case Some(a) => f(a)
      case None => None
