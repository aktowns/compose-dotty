import hedgehog._
import hedgehog.runner._

import tinyfp.typeclass.Functor

trait FunctorLaws[A[_]: Functor, X: PrimGen]:
  def gen: Gen[A[X]]

  def testIdentity: Property =
    for
      m <- gen.log("m")
    yield Functor[A].map(m)(identity) ==== m

  def testCompose: Property =
    for
      m <- gen.log("m")
      f <- PrimGen[X].fn1.log("f")
      g <- PrimGen[X].fn1.log("g")
    yield Functor[A].map(m)(f.andThen(g)) ==== (Functor[A].map(m)(f)).map(g)

  def tests: List[Test] = List(
    property("identity", testIdentity),
    property("compose", testCompose)
  )