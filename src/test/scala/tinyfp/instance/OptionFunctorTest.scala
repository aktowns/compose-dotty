package tinyfp.instance

import hedgehog._
import hedgehog.runner._

import tinyfp.laws._
import given tinyfp.laws._

import tinyfp.typeclass.Functor
import given tinyfp.instance.OptionInstances

object OptionFunctorTest extends Properties with FunctorLaws[Option, Int]:
  def gen: Gen[Option[Int]] =
    for
      v <- Gen.int(Range.linear(0, 100))
    yield if (v > 50) then Some(v) else None
