package typeclass

type ~>[F[_], G[_]] = NaturalTransformation[F, G]

trait NaturalTransformation[F[_], G[_]]