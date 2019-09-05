import hedgehog._
import hedgehog.runner._

import tinyfp.typeclass.Monoid
import given tinyfp.instance.VectorMonoid

object VectorMonoidTest extends Properties with MonoidLaws[Vector[Int]]:
  def gen: Gen[Vector[Int]] = 
    Gen.int(Range.linear(0,1000)).list(Range.linear(0, 100)).map(_.toVector)
