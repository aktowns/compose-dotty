package tinyfp.data

case class Kleisli[M[_], A, B](runKleisli: A => M[B])

