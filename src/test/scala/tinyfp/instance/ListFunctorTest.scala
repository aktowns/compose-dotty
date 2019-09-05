import hedgehog._
import hedgehog.runner._

import tinyfp.typeclass.Functor
import given tinyfp.instance.ListFunctor

object ListFunctorTest extends Properties with FunctorLaws[List, Int]:
  def gen: Gen[List[Int]] = Gen.int(Range.linear(0,1000)).list(Range.linear(0, 100))
