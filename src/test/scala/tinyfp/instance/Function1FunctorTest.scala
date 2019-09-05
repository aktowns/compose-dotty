import hedgehog._
import hedgehog.runner._

import tinyfp.typeclass.Functor
import given tinyfp.instance.Function1Functor

object Function1FunctorTest // TODO: Temp disable, might need an Eq? extends Properties with FunctorLaws[[X] =>> Function1[Int, X], String]:
  def gen: Gen[Function1[Int, String]] =
    for
      v <- Gen.int(Range.linear(0, 100))
    yield (x) => s"$v-$x"
