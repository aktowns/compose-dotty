import hedgehog._
import hedgehog.runner._

import tinyfp.typeclass.Monoid
import given tinyfp.instance.ListMonoid

object ListMonoidTest extends Properties with MonoidLaws[List[Int]]:
  def gen: Gen[List[Int]] = Gen.int(Range.linear(0,1000)).list(Range.linear(0, 100))