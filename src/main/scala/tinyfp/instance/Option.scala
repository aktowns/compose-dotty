package tinyfp.instance

import tinyfp.typeclass._

given OptionFunctor as Functor[Option]:
  def (fa: Option[A]) map [A, B] (f: A => B): Option[B] = 
    fa.fold(None)(x => Some(f(x))) 

given OptionApplicative as Applicative[Option] = OptionAlternative
    
given OptionAlternative as Alternative[Option]:
  def (fa: => Option[A]) <|> [A] (fb: => Option[A]) =
    (fa, fb) match
      case (None, r) => r
      case (l, _) => l
  def empty[A] = None

  // Applicative
  def (f: Option[A => B]) <*> [A, B] (fa: Option[A]): Option[B] = 
    (f, fa) match
      case (Some(ff), ffa) => ff <#> ffa
      case (None, _)       => None

    fa.map(x => f.map(y => y(x))).flatten
  def pure[A](a: A): Option[A] = Some(a)

given OptionMonad as Monad[Option]:
  def (fa: Option[A]) >>= [A, B] (f: A => Option[B]): Option[B] =
    fa match
      case Some(a) => f(a)
      case None => None

  override def pure[A](a: A): Option[A] = Some(a)



