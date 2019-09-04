package tinyfp.data

import tinyfp.typeclass._

case class Compose[F[_], G[_], A](getCompose: F[G[A]])

//given ComposeFunctor[F[_], G[_]] as Functor[[A] =>> Compose[F, G, A]]:
  