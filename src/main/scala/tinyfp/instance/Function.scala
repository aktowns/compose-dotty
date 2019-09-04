package tinyfp.instance

import tinyfp.typeclass._

given Function1Functor[A] as Functor[[B] =>> Function1[A, B]]:
  def (fa: A => B) map [B, C] (f: B => C): A => C = fa.andThen(f)