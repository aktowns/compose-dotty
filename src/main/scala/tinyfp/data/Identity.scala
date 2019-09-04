package tinyfp.data

import tinyfp.typeclass._

/** The identity functor and monad.

This trivial type constructor serves two purposes:

  - It can be used with functions parameterized by functor or monad classes.
  - It can be used as a base monad to which a series of monad transformers may be applied to construct a composite monad.
*/
case class Identity[A](runIdentity: A)

given IdentityFunctor as Functor[Identity]:
  def (fa: Identity[A]) map [A, B] (f: A => B): Identity[B] =
    Identity(f(fa.runIdentity))

given IdentityApplicative as Applicative[Identity]:
  def (f: Identity[A => B]) <*> [A, B] (fa: Identity[A]): Identity[B] =
    Identity(f.runIdentity(fa.runIdentity))

  def pure[A](a: A): Identity[A] = Identity(a)

given IdentityMonad as Monad[Identity]:
  def (x: Identity[A]) >>= [A, B] (f: A => Identity[B]): Identity[B] =
    f(x.runIdentity)

  def pure[A](a: A): Identity[A] = Identity(a)