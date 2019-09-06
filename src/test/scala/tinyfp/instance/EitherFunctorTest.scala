package tinyfp.instance

import hedgehog._
import hedgehog.runner._

import tinyfp.laws._
import given tinyfp.laws._

import tinyfp.typeclass.Functor
import given tinyfp.instance.EitherRightBiasedFunctor

object EitherFunctorTest extends Properties with FunctorLaws[[X] =>> Either[String, X], Int]:
  def gen: Gen[Either[String, Int]] =
    for
      v <- Gen.int(Range.linear(0, 100))
      y <- Gen.string(Gen.alpha, Range.linear(0, 10))
    yield if (v > 50) then Right(v) else Left(y)
