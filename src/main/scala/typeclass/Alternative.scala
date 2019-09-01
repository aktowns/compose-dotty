package typeclass

trait Alternative[F[_]] extends Applicative[F]:
  def (fa: F[A]) <|> [A] (fb: F[A]): F[A]
  def empty[A]: F[A]

  def (fa: F[A]) some [A]: F[List[A]] = 
    def many_v: F[List[A]] = some_v <|> pure(List[A]())
    def some_v: F[List[A]] = liftA2((x: A) => (y: List[A]) => x +: y)(fa)(many_v)
    some_v

  def (fa: F[A]) many [A]: F[List[A]] =
    def many_v: F[List[A]] = some_v <|> pure(List[A]())
    def some_v: F[List[A]] = liftA2((x: A) => (y: List[A]) => x +: y)(fa)(many_v)
    many_v

  def (fa: F[A]) optional [A]: F[Option[A]] = Some[A].apply <#> fa <|> pure(None)

object Alternative:
  def apply[T[_]] given Alternative[T] = the[Alternative[T]]
