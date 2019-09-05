import hedgehog._
import hedgehog.runner._

import tinyfp.typeclass.Functor
import given tinyfp.instance.VectorFunctor

object VectorFunctorTest extends Properties with FunctorLaws[Vector, Int]:
  def gen: Gen[Vector[Int]] = 
    Gen.int(Range.linear(0,1000)).list(Range.linear(0, 100)).map(_.toVector)
