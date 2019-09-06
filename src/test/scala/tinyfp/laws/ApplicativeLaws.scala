package tinyfp.laws

import hedgehog._
import hedgehog.runner._

import tinyfp.typeclass.Applicative
import given tinyfp.typeclass.Applicative

trait ApplicativeLaws[A[_]: Applicative, X: PrimGen]:
  def gen: Gen[A[X]]

  def testIdentity: Property =
    for
      m <- gen.log("m")
    yield Applicative[A].ap(Applicative[A].pure(identity[X]))(m) ==== m

  def testCompose: Property =
    for
      u <- PrimGen[X].fn1.log("u").map(Applicative[A].pure)
      v <- PrimGen[X].fn1.log("v").map(Applicative[A].pure)
      w <- gen.log("w")
    yield Applicative[A].pure((x: X => X) => (y: X => X) => x.andThen(y)) <*> u <*> v <*> w ==== u <*> (v <*> w)

  def tests: List[Test] = List(
    property("identity", testIdentity),
//    property("homomorphism", testHomomorphism),
//    property("interchange", testInterchange),
    property("compose", testCompose)
  )
