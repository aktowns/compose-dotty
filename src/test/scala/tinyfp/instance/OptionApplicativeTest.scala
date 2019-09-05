import hedgehog._
import hedgehog.runner._

import tinyfp.typeclass.Applicative
import given tinyfp.instance.OptionApplicative

object OptionApplicativeTest extends Properties with ApplicativeLaws[Option, Int]:
  def gen: Gen[Option[Int]] =
    for
      v <- Gen.int(Range.linear(0, 100))
    yield if (v > 50) then Some(v) else None
