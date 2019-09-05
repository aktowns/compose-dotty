import hedgehog._
import hedgehog.runner._

import tinyfp.typeclass.Monoid
import given tinyfp.instance.StringMonoid

object StringMonoidTest extends Properties with MonoidLaws[String]:
  def gen: Gen[String] = Gen.string(Gen.alpha, Range.linear(0, 100))