package tinyfp.data

import tinyfp.typeclass._

case class Compose[F[_]: Functor, G[_]: Functor, A](getCompose: F[G[A]])

given ComposeFunctor[F[_]: Functor, G[_]: Functor] as Functor[[A] =>> Compose[F, G, A]]:
  def (fa: Compose[F, G, A]) map [A, B] (f: A => B): Compose[F, G, B] = 
    Compose(fa.getCompose.map(x => x.map(f)))