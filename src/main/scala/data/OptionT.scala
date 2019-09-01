package data

case class OptionT[F[_], A](runOptionT: F[Option[A]])
