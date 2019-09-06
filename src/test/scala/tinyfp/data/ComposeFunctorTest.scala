package tinyfp.data

import hedgehog._
import hedgehog.runner._

import tinyfp.laws._
import given tinyfp.laws._
import tinyfp.instance.OptionFunctorTest
import tinyfp.instance.ListFunctorTest

import tinyfp.typeclass.Functor
import given tinyfp.instance._
import given tinyfp.data._

object ComposeFunctorTest extends Properties with FunctorLaws[[X] =>> Compose[Option, List, X], Int]:
  def gen: Gen[Compose[Option, List, Int]] =
    for
      x <-  Gen.int(Range.linear(0, 100))
      g <- ListFunctorTest.gen
      f <- OptionFunctorTest.gen
    yield Compose(f.map(_ => g.map(_ =>x)))
