package tinyfp.transformers

case class OptionT[F[_], A](runOptionT: F[Option[A]])
